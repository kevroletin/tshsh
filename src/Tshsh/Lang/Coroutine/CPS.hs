{-# LANGUAGE GADTs #-}
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
module Tshsh.Lang.Coroutine.CPS
  ( Program (..),
    ProgramSt,
    ProgramCont,
    ProgramCont_,
    ContResOut (..),
    _ContOut,
    _ResOut,
    ContRes (..),
    _Cont,
    _Res,
    step,
    whenC,
    unlessC,
    liftP_,
    finishP,
    waitInputC_,
  )
where

import Control.Lens
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
  -- a sequence of n pipes will add O(n) complexity for each step call;
  -- maybe we can improve using a zipper-like transformation to keep
  -- currently outputting program in focus (similarly to what we did
  -- with AndThen
  Pipe :: Program st i o' m -> Program st o' o m -> Program st i o m
  AdapterSt :: Lens' st st' -> (i -> Maybe i') -> (o' -> o) -> Program st' i' o' m -> Program st i o m
  Adapter :: (i -> Maybe i') -> (o' -> o) -> Program st i' o' m -> Program st i o m
  AndThen :: Program st i o m -> Program st i o m -> Program st i o m

type ProgramSt st i o m = Pair st (Program st i o m)

type ProgramCont st i o m s = (s -> Program st i o m) -> Program st i o m

type ProgramCont_ st i o m = Program st i o m -> Program st i o m

data ContResOut st i o m
  = ContOut (Maybe o) (Pair st (Program st i o m))
  | ResOut (Pair st (Either Error ()))

$(makePrisms 'ResOut)

data ContRes st i o m
  = Cont (Pair st (Program st i o m))
  | Res (Pair st (Either Error ()))

$(makePrisms 'Res)

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

deriving instance (Show st, Show i, Show o) => Show (ContResOut st i o m)

deriving instance (Show st, Show i, Show o) => Show (ContRes st i o m)

step ::
  forall st i o m.
  Monad m =>
  Maybe i ->
  Pair st (Program st i o m) ->
  m (ContResOut st i o m)
step i (st :!: Lift ma cont) = do
  a <- ma
  step i (st :!: cont a)
step i (st :!: GetState cont) = step i (st :!: cont st)
step i (_ :!: PutState st cont) = step i (st :!: cont)
step i (st :!: ModifyState f cont) = step i (f st :!: cont)
step (Just i) (st :!: WaitInput cont) = step Nothing (st :!: cont i)
step Nothing x@(_ :!: WaitInput _) = pure $ ContOut Nothing x
step Nothing (st :!: Output x next) = pure $ ContOut (Just x) (st :!: next)
step (Just _) (_st :!: Output _ _) = panic "Consume all the outputs first"
step Nothing (st :!: Finish a) = pure $ ResOut (st :!: a)
step (Just _) (_st :!: Finish _) = panic "Consume all the outputs before reading a result"
step mi (st0 :!: Pipe p1_ p2_) =
  let feedP2 st p1 p2 i =
        step i (st :!: p2) >>= \case
          ContOut (Just o) (newSt :!: newP2) ->
            pure $ ContOut (Just o) (newSt :!: Pipe p1 newP2)
          ContOut Nothing (newSt :!: newP2) ->
            feedP1 newSt p1 newP2 Nothing
          ResOut res -> pure $ ResOut res
      feedP1 st p1 p2 i =
        step i (st :!: p1) >>= \case
          ContOut (Just o) (newSt :!: newP1) -> feedP2 newSt newP1 p2 (Just o)
          ContOut Nothing (newSt :!: newP1) ->
            pure $ ContOut Nothing (newSt :!: Pipe newP1 p2)
          ResOut res -> pure $ ResOut res
   in case mi of
        Nothing ->
          feedP2 st0 p1_ p2_ Nothing
        Just i ->
          feedP1 st0 p1_ p2_ (Just i)
step mi (st :!: Adapter proj inj p) =
  step (proj =<< mi) (st :!: p) >>= \case
    ContOut o (newSt :!: newP) ->
      pure $ ContOut (inj <$> o) (newSt :!: Adapter proj inj newP)
    ResOut res -> pure $ ResOut res
step mi (st :!: AdapterSt stLens proj inj p) =
  step (proj =<< mi) (st ^. stLens :!: p) >>= \case
    ContOut o (newSt :!: newP) ->
      pure $ ContOut (inj <$> o) ((st & stLens .~ newSt) :!: AdapterSt stLens proj inj newP)
    ResOut (newSt :!: res) ->
      pure $ ResOut ((st & stLens .~ newSt) :!: res)
step i (st :!: AndThen (AndThen a b) c) = step i (st :!: AndThen a (AndThen b c))
step i (st :!: AndThen p1 p2) =
  step i (st :!: p1) >>= \case
    ContOut o (newSt :!: newP1) -> pure $ ContOut o (newSt :!: AndThen newP1 p2)
    ResOut (newSt :!: Left err) -> pure $ ResOut (newSt :!: Left err)
    ResOut (newSt :!: Right ()) -> step Nothing (newSt :!: p2)

whenC :: Bool -> (a -> a) -> a -> a
whenC False _ cont = cont
whenC True act cont = act cont
{-# INLINE whenC #-}

unlessC :: Bool -> (a -> a) -> a -> a
unlessC True _ cont = cont
unlessC False act cont = act cont
{-# INLINE unlessC #-}

liftP_ :: m a -> Program st i o m -> Program st i o m
liftP_ act cont = Lift act $ const cont
{-# INLINE liftP_ #-}

finishP :: Program st i o m
finishP = Finish (Right ())
{-# INLINE finishP #-}

waitInputC_ :: ProgramCont_ st i o m
waitInputC_ cont = WaitInput $ \_ -> cont
{-# INLINE waitInputC_ #-}
