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
    ProgramTimeout (..),
    StepEnv (..),
    pipe,
    unProgramEv,
    ProgramLike (..),
    Ev (..),
    ContResOut (..),
    _ContOut,
    _ResOut,
    getNextTimeout,
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
import Data.Time.Clock
import Protolude hiding (pi)
import Prelude (Show (..))

type Error = Text

data PipeList st i o m r where
  PipeCons :: Program st i o' m r -> PipeList st o' o m r -> PipeList st i o m r
  PipeNil :: PipeList st o o m r

data PipeRevList st i o m r where
  PipeRevSnoc :: PipeRevList st i o' m r -> Program st o' o m r -> PipeRevList st i o m r
  PipeRevNil :: PipeRevList st i i m r

data ProgramTimeout
  = TimeoutRelative NominalDiffTime
  | TimeoutAbsolute UTCTime
  | TimeoutInfinite
  deriving (Eq, Ord, Show)

-- st - state (for GetState, Put state)
-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- m - base Monad for Lift (and for the interpreter 'step')
data Program st i o m r where
  Lift :: m a -> (a -> Program st i o m r) -> Program st i o m r
  GetState :: (st -> Program st i o m r) -> Program st i o m r
  PutState :: st -> ~(Program st i o m r) -> Program st i o m r
  ModifyState :: (st -> st) -> ~(Program st i o m r) -> Program st i o m r
  WaitInput :: (i -> Program st i o m r) -> Program st i o m r
  -- WaitInput :: ProgramTimeout -> (i -> Program st i o m r) -> Program st i o m r -> Program st i o m r
  WaitTime :: ProgramTimeout -> Program st i o m r -> Program st i o m r
  Output :: o -> ~(Program st i o m r) -> Program st i o m r
  Finish :: Either Error r -> Program st i o m r
  -- (Pipe prev mo next) is a zipper where
  -- prev - list of Programs in 'NotEv state (they potentially can output from stepOut)
  -- mo - pending input to the first program in next list
  -- next - list of Programs in 'Ev state (they are ready to consume input by stepIn)
  Pipe :: PipeRevList st i o' m r -> Maybe o' -> PipeList st o' o m r -> Program st i o m r
  AdapterAll :: Lens' st st' -> (i -> Maybe i') -> (o' -> o) -> Program st' i' o' m r -> Program st i o m r
  AdapterSt :: Lens' st st' -> Program st' i o m r -> Program st i o m r
  Adapter :: (i -> Maybe i') -> (o' -> o) -> Program st i' o' m r -> Program st i o m r
  AndThen :: Program st i o m s -> (s -> Program st i o m t) -> Program st i o m t
  BuffInput :: Maybe i -> Program st i o m r -> Program st i o m r

pipeListAppend :: PipeList st i o' m r -> PipeList st o' o m r -> PipeList st i o m r
pipeListAppend PipeNil next = next
pipeListAppend (PipeCons p prev) next = PipeCons p (pipeListAppend prev next)

pipe :: Program st i o' m r -> Program st o' o m r -> Program st i o m r
pipe (Pipe PipeRevNil Nothing prev) (Pipe PipeRevNil Nothing next) =
  Pipe PipeRevNil Nothing (pipeListAppend prev next)
pipe p (Pipe PipeRevNil Nothing ps) = Pipe PipeRevNil Nothing (PipeCons p ps)
pipe (Pipe PipeRevNil Nothing prev) p =
  Pipe PipeRevNil Nothing (pipeListAppend prev (PipeCons p PipeNil))
pipe p1 p2 = Pipe (PipeRevSnoc (PipeRevSnoc PipeRevNil p1) p2) Nothing PipeNil

infixr 9 `pipe`

instance (Show o) => Show (Program st i o m r) where
  show (Lift _ _) = "Lift"
  show (GetState _) = "GetState"
  show (PutState _ _) = "PutState"
  show (ModifyState _ _) = "ModifyState"
  show (WaitInput _) = "WaitInput"
  show (WaitTime time _) = "WaitTime " <> (Prelude.show time)
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

newtype ProgramEv (ev :: Ev) st i o m r = PEv (Program st i o m r)
  deriving (Show)

unProgramEv :: ProgramEv ev st i o m r -> Program st i o m r
unProgramEv = coerce
{-# INLINE unProgramEv #-}

data ContResOut st i o m r where
  ContOut :: o -> Pair st (ProgramEv 'NotEv st i o m r) -> ContResOut st i o m r
  ContNoOut :: Pair st (ProgramEv 'Ev st i o m r) -> ContResOut st i o m r
  ResOut :: Pair st (Either Error r) -> ContResOut st i o m r

$(makePrisms 'ResOut)

deriving instance (Show st, Show i, Show o, Show r) => Show (ContResOut st i o m r)

newtype StepEnv = StepEnv UTCTime deriving (Eq, Ord, Show)

stepUnsafe ::
  forall st i o m r.
  Monad m =>
  StepEnv ->
  Maybe i ->
  Pair st (Program st i o m r) ->
  m (ContResOut st i o m r)
stepUnsafe env i (st :!: Lift ma cont) = do
  a <- ma
  stepUnsafe env i (st :!: cont a)
stepUnsafe env i (st :!: GetState cont) = stepUnsafe env i (st :!: cont st)
stepUnsafe env i (_ :!: PutState st cont) = stepUnsafe env i (st :!: cont)
stepUnsafe env i (st :!: ModifyState f cont) = stepUnsafe env i (f st :!: cont)
stepUnsafe env (Just i) (st :!: WaitInput cont) = stepUnsafe env Nothing (st :!: cont i)
stepUnsafe _env Nothing x@(_ :!: WaitInput _) = pure $ ContNoOut (coerce x)
stepUnsafe _env Nothing (st :!: Output x next) = pure $ ContOut x (st :!: coerce next)
stepUnsafe _env (Just _) (_st :!: Output _ _) = panic "Consume all the outputs first"
stepUnsafe _env Nothing (st :!: Finish a) = pure $ ResOut (st :!: a)
stepUnsafe _env (Just _) (_st :!: Finish _) = panic "Consume all the outputs before reading a result"
stepUnsafe _env Nothing (st0 :!: (Pipe prev (Just o) PipeNil)) =
  pure $ ContOut o (st0 :!: coerce (Pipe prev Nothing PipeNil))
stepUnsafe env Nothing (st0 :!: (Pipe prev (Just i) (PipeCons p next))) =
  stepUnsafe env (Just i) (st0 :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      stepUnsafe env Nothing (newSt :!: Pipe (PipeRevSnoc prev newP) (Just o) next)
    ContNoOut (newSt :!: PEv newP) ->
      stepUnsafe env Nothing (newSt :!: Pipe prev Nothing (PipeCons newP next))
    ResOut res -> pure $ ResOut res
stepUnsafe env Nothing (st0 :!: (Pipe (PipeRevSnoc prev p) Nothing next)) =
  stepUnsafe env Nothing (st0 :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      stepUnsafe env Nothing (newSt :!: Pipe (PipeRevSnoc prev newP) (Just o) next)
    ContNoOut (newSt :!: PEv newP) ->
      stepUnsafe env Nothing (newSt :!: Pipe prev Nothing (PipeCons newP next))
    ResOut res -> pure $ ResOut res
stepUnsafe _env Nothing (st0 :!: p0@(Pipe PipeRevNil Nothing _)) =
  pure $ ContNoOut (st0 :!: coerce p0)
stepUnsafe env (Just i) (st0 :!: Pipe PipeRevNil Nothing (PipeCons p ps)) =
  stepUnsafe env Nothing (st0 :!: Pipe PipeRevNil (Just i) (PipeCons p ps))
stepUnsafe _env (Just _) (_ :!: Pipe {}) =
  panic "Consume all Pipe outputs first"
stepUnsafe env mi (st :!: Adapter proj inj p) =
  stepUnsafe env (proj =<< mi) (st :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut (inj o) (newSt :!: coerce (Adapter proj inj newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut (newSt :!: coerce (Adapter proj inj newP))
    ResOut res -> pure $ ResOut res
stepUnsafe env mi (st :!: AdapterSt stLens p) =
  stepUnsafe env mi (st ^. stLens :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut o ((st & stLens .~ newSt) :!: coerce (AdapterSt stLens newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut ((st & stLens .~ newSt) :!: coerce (AdapterSt stLens newP))
    ResOut (newSt :!: res) ->
      pure $ ResOut ((st & stLens .~ newSt) :!: res)
stepUnsafe env mi (st :!: AdapterAll stLens proj inj p) =
  stepUnsafe env (proj =<< mi) (st ^. stLens :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut (inj o) ((st & stLens .~ newSt) :!: coerce (AdapterAll stLens proj inj newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut ((st & stLens .~ newSt) :!: coerce (AdapterAll stLens proj inj newP))
    ResOut (newSt :!: res) ->
      pure $ ResOut ((st & stLens .~ newSt) :!: res)
stepUnsafe env i (st :!: AndThen (AndThen a b) c) =
  stepUnsafe env i (st :!: AndThen a (\r -> AndThen (b r) c))
stepUnsafe env i (st :!: AndThen p1 p2) =
  stepUnsafe env i (st :!: p1) >>= \case
    ContOut o (newSt :!: PEv newP1) -> pure $ ContOut o (newSt :!: coerce (AndThen newP1 p2))
    ContNoOut (newSt :!: PEv newP1) -> pure $ ContNoOut (newSt :!: coerce (AndThen newP1 p2))
    ResOut (newSt :!: Left err) -> pure $ ResOut (newSt :!: Left err)
    ResOut (newSt :!: Right r) ->
      if isJust i && not (isEv p1)
        then panic "Consume all the outputs before evaluating AndThen"
        else stepUnsafe env Nothing (newSt :!: (p2 r))
stepUnsafe env i (st :!: BuffInput Nothing p) =
  stepUnsafe env Nothing (st :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) -> pure $ ContOut o (newSt :!: coerce (BuffInput Nothing newP))
    ContNoOut (newSt :!: PEv newP) -> stepUnsafe env i (newSt :!: newP)
    ResOut res -> pure $ ResOut res
stepUnsafe env Nothing (st :!: BuffInput (Just i) p) = stepUnsafe env (Just i) (st :!: p)
stepUnsafe env (Just newInp) (st :!: BuffInput (Just oldInp) p) =
  stepUnsafe env (Just oldInp) (st :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut o (newSt :!: coerce (BuffInput (Just newInp) newP))
    ContNoOut (newSt :!: PEv newP) ->
      stepUnsafe env (Just newInp) (newSt :!: newP)
    ResOut res ->
      pure $ ResOut (coerce res)
stepUnsafe env@(StepEnv currTime) mi (st :!: WaitTime timeout next) =
  case timeout of
    TimeoutRelative diffTime ->
      stepUnsafe env mi (st :!: WaitTime (TimeoutAbsolute (addUTCTime diffTime currTime)) next)
    TimeoutAbsolute utc ->
      if currTime <= utc
        then pure $ ContNoOut (st :!: coerce (WaitTime timeout next))
        else stepUnsafe env Nothing (st :!: BuffInput mi (next))
    TimeoutInfinite -> stepUnsafe env mi (st :!: next)
{-# INLINEABLE stepUnsafe #-}

data EvWitness st i o m r where
  EvWitness :: ProgramEv 'Ev st i o m r -> EvWitness st i o m r
  NotEvWitness :: ProgramEv 'NotEv st i o m r -> EvWitness st i o m r

getNextTimeout :: Program st i o m r -> Maybe UTCTime
getNextTimeout (WaitTime (TimeoutAbsolute utc) _) = Just utc
getNextTimeout _ = Nothing

isEv :: Program st i o m r -> Bool
isEv Lift {} = False
isEv GetState {} = False
isEv PutState {} = False
isEv ModifyState {} = False
isEv WaitInput {} = True
isEv Output {} = False
isEv Finish {} = False
isEv (Pipe PipeRevNil _ _) = True
isEv (Pipe _ _ xs0) =
  let go :: PipeList st i o m r -> Bool
      go PipeNil = True
      go (PipeCons x xs) = isEv x && go xs
   in go xs0
isEv (AdapterAll _ _ _ p) = isEv p
isEv (Adapter _ _ p) = isEv p
isEv (AndThen p1 _) = isEv p1
isEv (BuffInput Nothing p) = isEv p
isEv _ = False
{-# INLINE isEv #-}

matchEv :: Program st i o m r -> EvWitness st i o m r
matchEv p
  | isEv p = EvWitness (PEv p)
  | otherwise = NotEvWitness (PEv p)
{-# INLINE matchEv #-}

toEv :: Program st i o m r -> ProgramEv 'Ev st i o m r
toEv p = coerce (BuffInput Nothing p)
{-# INLINE toEv #-}

-- Both tagged and untagged programs can be evaluated to 'Ev form without
-- providing any input. This class makes it possible to write one polymorphic
-- implementation for tagged and untagged Program.
class ProgramLike p st i o m r where
  stepOut :: Monad m => StepEnv -> Pair st (p st i o m r) -> m (ContResOut st i o m r)

instance ProgramLike Program st i o m r where
  stepOut = stepOutP

instance ProgramLike (ProgramEv ev) st i o m r where
  stepOut = stepOutEv

stepInput ::
  forall st i o m r.
  Monad m =>
  StepEnv ->
  i ->
  Pair st (ProgramEv 'Ev st i o m r) ->
  m (ContResOut st i o m r)
stepInput env i p = stepUnsafe env (Just i) (coerce p)
{-# INLINE stepInput #-}

stepOutEv ::
  forall ev st i o m r.
  Monad m =>
  StepEnv ->
  Pair st (ProgramEv ev st i o m r) ->
  m (ContResOut st i o m r)
stepOutEv env p = stepUnsafe env Nothing (coerce p)
{-# INLINE stepOutEv #-}

stepOutP ::
  forall st i o m r.
  Monad m =>
  StepEnv ->
  Pair st (Program st i o m r) ->
  m (ContResOut st i o m r)
stepOutP env = stepUnsafe env Nothing
{-# INLINE stepOutP #-}
