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
-- types. In this case one can use Adapter, AdapterSt and AndThen.
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
  )
where

import Control.Lens
import Data.Coerce
import Data.Strict.Tuple
import Protolude hiding (pi)
import Prelude (Show (..))

type Error = Text

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
  -- a sequence of n pipes will add O(n) complexity for each stepUnsafe call;
  -- maybe we can improve using a zipper-like transformation to keep
  -- currently outputting program in focus (similarly to what we did
  -- with AndThen
  Pipe :: Program st i o' m -> Program st o' o m -> Program st i o m
  AdapterSt :: Lens' st st' -> (i -> Maybe i') -> (o' -> o) -> Program st' i' o' m -> Program st i o m
  Adapter :: (i -> Maybe i') -> (o' -> o) -> Program st i' o' m -> Program st i o m
  AndThen :: Program st i o m -> Program st i o m -> Program st i o m

instance (Show o) => Show (Program st i o m) where
  show (Lift _ _) = "Lift"
  show (GetState _) = "GetState"
  show (PutState _ _) = "PutState"
  show (ModifyState _ _) = "ModifyState"
  show (WaitInput _) = "WaitInput"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish"
  show (Pipe _ _) = "Pipe"
  show Adapter {} = "Adapter"
  show AdapterSt {} = "AdapterSt"
  show AndThen {} = "AndThen"

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
stepUnsafe mi (st0 :!: Pipe p1_ p2_) =
  let feedP2 st p1 p2 i =
        stepUnsafe i (st :!: p2) >>= \case
          ContOut o (newSt :!: PEv newP2) ->
            pure $ ContOut o (newSt :!: coerce (Pipe p1 newP2))
          ContNoOut (newSt :!: PEv newP2) ->
            feedP1 newSt p1 newP2 Nothing
          ResOut res ->
            pure $ ResOut res
      feedP1 st p1 p2 i =
        stepUnsafe i (st :!: p1) >>= \case
          ContOut o (newSt :!: PEv newP1) ->
            feedP2 newSt newP1 p2 (Just o)
          ContNoOut (newSt :!: PEv newP1) ->
            pure $ ContNoOut (newSt :!: coerce (Pipe newP1 p2))
          ResOut res ->
            pure $ ResOut res
   in case mi of
        Nothing ->
          feedP2 st0 p1_ p2_ Nothing
        Just i ->
          feedP1 st0 p1_ p2_ (Just i)
stepUnsafe mi (st :!: Adapter proj inj p) =
  stepUnsafe (proj =<< mi) (st :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut (inj o) (newSt :!: coerce (Adapter proj inj newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut (newSt :!: coerce (Adapter proj inj newP))
    ResOut res -> pure $ ResOut res
stepUnsafe mi (st :!: AdapterSt stLens proj inj p) =
  stepUnsafe (proj =<< mi) (st ^. stLens :!: p) >>= \case
    ContOut o (newSt :!: PEv newP) ->
      pure $ ContOut (inj o) ((st & stLens .~ newSt) :!: coerce (AdapterSt stLens proj inj newP))
    ContNoOut (newSt :!: PEv newP) ->
      pure $ ContNoOut ((st & stLens .~ newSt) :!: coerce (AdapterSt stLens proj inj newP))
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
isEv (Pipe p1 p2) = isEv p1 && isEv p2
isEv (AdapterSt _ _ _ p) = isEv p
isEv (Adapter _ _ p) = isEv p
isEv (AndThen p1 _) = isEv p1
{-# INLINE isEv #-}

matchEv :: Program st i o m -> EvWitness st i o m
matchEv p
  | isEv p = EvWitness (PEv p)
  | otherwise = NotEvWitness (PEv p)
{-# INLINE matchEv #-}

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
