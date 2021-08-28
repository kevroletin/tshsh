{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- An implementation of coroutines. It's features are:
-- + interprets to a monadic value, provides Lift operation
-- + built-in state
-- + two ways of composition:
--   + continuation passing style
--   + using `AndThen` construction
--
-- Unlike conventional Monad-based composition, Program doesn't return a value
-- so 'm a -> (a -> m b) -> m b' style of composition can be achieved using CPS
-- style composition.
--
-- Although we provide AndThen combinator, and it should be quite fast, in many
-- cases it can be replaces with CPS style of programming. However, one area
-- where CPS fails is composing programs with different state/input/output
-- types. In this case one can use Adapter, AdapterAll and AndThen.
--
-- We've optimized AndThen so that both constructing programs (similar to
-- monodic >>=) and evaluating a single node should have O(1) amortized
-- complexity (worst case is O(n), but consecutive calls will be O(1)).
--
-- While evaluating a program we rotate nested `andThen` like this:
--
-- (3)    b->c                   a->b
--       /    \                 /   \
-- (2) a->b    c    into       a    b->c
--     /  \                         / \
-- (1)a    b                       b   c
--
-- This improves complexity of evaluating a program into a coroutine.
-- Coroutine interpreter evaluates one node at a time and returns the rest
-- of program tree intact. We always evaluate leftmost leave; evaluating one
-- node rebuilds all the nodes on the path to the root. In the example above if
-- (1) yields then interpreter will rebuild (2) and (3). This leads to O(d^2)
-- complexity where d is depth of an evaluated node.
--
-- Doing transformation above can be viewed as using a zipper to focus on the
-- currently evaluated node.
module Tshsh.Lang.Coroutine.CPS.Internal
  ( Program (..),
    ProgramEv (..),
    pipe,
    unProgramEv,
    ProgramLike (..),
    Ev (..),
    ContResOut (..),
    _ContOut,
    _ResOut,
    stepUnsafe,
    stepInput,
    EvWitness (..),
    matchEv,
    toEv,
  )
where

import Control.Lens
import Data.Coerce
import Data.Strict.Tuple
import Protolude hiding (pi)
import Prelude (Show (..))

type Error = Text

data PipeList st i o m where
  PipeCons :: Program st i o' m -> PipeList st o' o m -> PipeList st i o m
  PipeNil :: PipeList st o o m

data PipeRevList st i o m where
  PipeRevSnoc :: PipeRevList st i o' m -> Program st o' o m -> PipeRevList st i o m
  PipeRevNil :: PipeRevList st i i m

-- st - state (for GetState, Put state)
-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- m - base Monad for Lift (and for the interpreter 'step')
data Program st i o m where
  Lift :: m a -> (a -> Program st i o m) -> Program st i o m
  GetState :: (st -> Program st i o m) -> Program st i o m
  PutState :: st -> Program st i o m -> Program st i o m
  ModifyState :: (st -> st) -> Program st i o m -> Program st i o m
  WaitInput :: (i -> Program st i o m) -> Program st i o m
  Output :: o -> Program st i o m -> Program st i o m
  Finish :: Either Error () -> Program st i o m
  -- (Pipe prev mo next) is a zipper where
  -- prev - list of Programs in 'NotEv state (they potentially can output from stepOut)
  -- mo - pending input to the first program in next list
  -- next - list of Programs in 'Ev state (they are ready to consume input by stepIn)
  Pipe :: PipeRevList st i o' m -> Maybe o' -> PipeList st o' o m -> Program st i o m
  AdapterAll :: Lens' st st' -> (i -> Maybe i') -> (o' -> o) -> Program st' i' o' m -> Program st i o m
  AdapterSt :: Lens' st st' -> Program st' i o m -> Program st i o m
  Adapter :: (i -> Maybe i') -> (o' -> o) -> Program st i' o' m -> Program st i o m
  AndThen :: Program st i o m -> Program st i o m -> Program st i o m
  BuffInput :: Maybe i -> Program st i o m -> Program st i o m

pipeListAppend :: PipeList st i o' m -> PipeList st o' o m -> PipeList st i o m
pipeListAppend PipeNil next = next
pipeListAppend (PipeCons p prev) next = PipeCons p (pipeListAppend prev next)

pipe :: Program st i o' m -> Program st o' o m -> Program st i o m
pipe (Pipe PipeRevNil Nothing prev) (Pipe PipeRevNil Nothing next) =
  Pipe PipeRevNil Nothing (pipeListAppend prev next)
pipe p (Pipe PipeRevNil Nothing ps) = Pipe PipeRevNil Nothing (PipeCons p ps)
pipe (Pipe PipeRevNil Nothing prev) p =
  Pipe PipeRevNil Nothing (pipeListAppend prev (PipeCons p PipeNil))
pipe p1 p2 = pipe_ (PipeCons p1 (PipeCons p2 PipeNil))

infixr 9 `pipe`

pipe_ :: PipeList st o' o m -> Program st o' o m
pipe_ = Pipe PipeRevNil Nothing

instance (Show o) => Show (Program st i o m) where
  show (Lift _ _) = "Lift"
  show (GetState _) = "GetState"
  show (PutState _ _) = "PutState"
  show (ModifyState _ _) = "ModifyState"
  show (WaitInput _) = "WaitInput"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish"
  show Pipe {} = "Pipe"
  show Adapter {} = "Adapter"
  show AdapterAll {} = "AdapterAll"
  show AdapterSt {} = "AdapterSt"
  show AndThen {} = "AndThen"
  show BuffInput {} = "BuffInp"

-- 'Ev - program has been evaluated until WaitInput and cannot be further reduced without providing input
-- 'NotEv - program can be reduced without providing input
data Ev = Ev | NotEv

newtype ProgramEv (ev :: Ev) st i o m = PEv (Program st i o m)
  deriving (Show)

unProgramEv :: ProgramEv ev st i o m -> Program st i o m
unProgramEv = coerce
{-# INLINE unProgramEv #-}

data ContResOut st i o m where
  ContOut :: o -> Pair st (ProgramEv 'NotEv st i o m) -> ContResOut st i o m
  ContNoOut :: Pair st (ProgramEv 'Ev st i o m) -> ContResOut st i o m
  ResOut :: Pair st (Either Error ()) -> ContResOut st i o m

$(makePrisms 'ResOut)

deriving instance (Show st, Show i, Show o) => Show (ContResOut st i o m)

stepUnsafe ::
  forall st i o m.
  Monad m =>
  Maybe i ->
  Pair st (Program st i o m) ->
  m (ContResOut st i o m)
stepUnsafe i (st :!: Lift ma cont) = do
  a <- ma
  stepUnsafe i (st :!: cont a)
stepUnsafe i (st :!: GetState cont) = stepUnsafe i (st :!: cont st)
stepUnsafe i (_ :!: PutState st cont) = stepUnsafe i (st :!: cont)
stepUnsafe i (st :!: ModifyState f cont) = stepUnsafe i (f st :!: cont)
stepUnsafe (Just i) (st :!: WaitInput cont) = stepUnsafe Nothing (st :!: cont i)
stepUnsafe Nothing x@(_ :!: WaitInput _) = pure $ ContNoOut (coerce x)
stepUnsafe Nothing (st :!: Output x next) = pure $ ContOut x (st :!: coerce next)
stepUnsafe (Just _) (_st :!: Output _ _) = panic "Consume all the outputs first"
stepUnsafe Nothing (st :!: Finish a) = pure $ ResOut (st :!: a)
stepUnsafe (Just _) (_st :!: Finish _) = panic "Consume all the outputs before reading a result"
stepUnsafe Nothing (st0 :!: (Pipe prev (Just o) PipeNil)) =
  pure $ ContOut o (st0 :!: coerce (Pipe prev Nothing PipeNil))
stepUnsafe Nothing (st0 :!: (Pipe prev (Just i) (PipeCons p next))) =
  stepUnsafe (Just i) (st0 :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      stepUnsafe Nothing (newSt :!: Pipe (PipeRevSnoc prev newP) (Just o) next)
    ContNoOut (newSt :!: PEv newP) ->
      stepUnsafe Nothing (newSt :!: Pipe prev Nothing (PipeCons newP next))
    ResOut res -> pure $ ResOut res
stepUnsafe Nothing (st0 :!: (Pipe (PipeRevSnoc prev p) Nothing next)) =
  stepUnsafe Nothing (st0 :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      stepUnsafe Nothing (newSt :!: Pipe (PipeRevSnoc prev newP) (Just o) next)
    ContNoOut (newSt :!: PEv newP) ->
      stepUnsafe Nothing (newSt :!: Pipe prev Nothing (PipeCons newP next))
    ResOut res -> pure $ ResOut res
stepUnsafe Nothing (st0 :!: p0@(Pipe PipeRevNil Nothing _)) =
  pure $ ContNoOut (st0 :!: coerce p0)
stepUnsafe (Just i) (st0 :!: Pipe PipeRevNil Nothing (PipeCons p ps)) =
  stepUnsafe Nothing (st0 :!: Pipe PipeRevNil (Just i) (PipeCons p ps))
stepUnsafe (Just _) (_ :!: Pipe {}) =
  panic "Consume all Pipe outputs first"
stepUnsafe mi (st :!: Adapter proj inj p) =
  stepUnsafe (proj =<< mi) (st :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut (inj o) (newSt :!: coerce (Adapter proj inj newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut (newSt :!: coerce (Adapter proj inj newP))
    ResOut res -> pure $ ResOut res
stepUnsafe mi (st :!: AdapterSt stLens p) =
  stepUnsafe mi (st ^. stLens :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut o ((st & stLens .~ newSt) :!: coerce (AdapterSt stLens newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut ((st & stLens .~ newSt) :!: coerce (AdapterSt stLens newP))
    ResOut (newSt :!: res) ->
      pure $ ResOut ((st & stLens .~ newSt) :!: res)
stepUnsafe mi (st :!: AdapterAll stLens proj inj p) =
  stepUnsafe (proj =<< mi) (st ^. stLens :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut (inj o) ((st & stLens .~ newSt) :!: coerce (AdapterAll stLens proj inj newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut ((st & stLens .~ newSt) :!: coerce (AdapterAll stLens proj inj newP))
    ResOut (newSt :!: res) ->
      pure $ ResOut ((st & stLens .~ newSt) :!: res)
stepUnsafe i (st :!: AndThen (AndThen a b) c) = stepUnsafe i (st :!: AndThen a (AndThen b c))
stepUnsafe i (st :!: AndThen p1 p2) =
  stepUnsafe i (st :!: p1) >>= \case
    ContOut o (newSt :!: PEv newP1) -> pure $ ContOut o (newSt :!: coerce (AndThen newP1 p2))
    ContNoOut (newSt :!: PEv newP1) -> pure $ ContNoOut (newSt :!: coerce (AndThen newP1 p2))
    ResOut (newSt :!: Left err) -> pure $ ResOut (newSt :!: Left err)
    ResOut (newSt :!: Right ()) ->
      if isJust i && not (isEv p1)
        then panic "Consume all the outputs before evaluating AndThen"
        else stepUnsafe Nothing (newSt :!: p2)
stepUnsafe i (st :!: BuffInput Nothing p) = stepUnsafe i (st :!: p)
stepUnsafe Nothing (st :!: BuffInput (Just i) p) = stepUnsafe (Just i) (st :!: p)
stepUnsafe (Just newInp) (st :!: BuffInput (Just oldInp) p) =
  stepUnsafe (Just oldInp) (st :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut o (newSt :!: coerce (BuffInput (Just newInp) newP))
    ContNoOut (newSt :!: PEv newP) ->
      stepUnsafe (Just newInp) (newSt :!: newP)
    ResOut res ->
      pure $ ResOut (coerce res)
{-# INLINEABLE stepUnsafe #-}

data EvWitness st i o m where
  EvWitness :: ProgramEv 'Ev st i o m -> EvWitness st i o m
  NotEvWitness :: ProgramEv 'NotEv st i o m -> EvWitness st i o m

isEv :: Program st i o m -> Bool
isEv Lift {} = False
isEv GetState {} = False
isEv PutState {} = False
isEv ModifyState {} = False
isEv WaitInput {} = True
isEv Output {} = False
isEv Finish {} = False
isEv (Pipe PipeRevNil _ _) = True
isEv (Pipe _ _ xs0) =
  let go :: PipeList st i o m -> Bool
      go PipeNil = True
      go (PipeCons x xs) = isEv x && go xs
   in go xs0
isEv (AdapterAll _ _ _ p) = isEv p
isEv (Adapter _ _ p) = isEv p
isEv (AndThen p1 _) = isEv p1
isEv (BuffInput Nothing p) = isEv p
isEv _ = False
{-# INLINE isEv #-}

matchEv :: Program st i o m -> EvWitness st i o m
matchEv p
  | isEv p = EvWitness (PEv p)
  | otherwise = NotEvWitness (PEv p)
{-# INLINE matchEv #-}

toEv :: Program st i o m -> ProgramEv 'Ev st i o m
toEv p = coerce (BuffInput Nothing p)
{-# INLINE toEv #-}

-- Both tagged and untagged programs can be evaluated to 'Ev form without
-- providing any input. This class makes it possible to write one polymorphic
-- implementation for tagged and untagged Program.
class ProgramLike p st i o m where
  stepOut :: Monad m => Pair st (p st i o m) -> m (ContResOut st i o m)

instance ProgramLike Program st i o m where
  stepOut = stepOutP

instance ProgramLike (ProgramEv ev) st i o m where
  stepOut = stepOutEv

stepInput ::
  forall st i o m.
  Monad m =>
  i ->
  Pair st (ProgramEv 'Ev st i o m) ->
  m (ContResOut st i o m)
stepInput i p = stepUnsafe (Just i) (coerce p)
{-# INLINE stepInput #-}

stepOutEv ::
  forall ev st i o m.
  Monad m =>
  Pair st (ProgramEv ev st i o m) ->
  m (ContResOut st i o m)
stepOutEv p = stepUnsafe Nothing (coerce p)
{-# INLINE stepOutEv #-}

stepOutP ::
  forall st i o m.
  Monad m =>
  Pair st (Program st i o m) ->
  m (ContResOut st i o m)
stepOutP = stepUnsafe Nothing
{-# INLINE stepOutP #-}
