{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- An implementation of coroutines. It's features are:
-- + simple implementation under 100loc
-- + interprets to a monadic value, provides Lift operation
-- + built-in state
module Lang.Coroutine.CPS
  ( Program (..),
    ProgramSt,
    ProgramCont,
    ProgramAdaptCont,
    ProgramCont',
    ProgramAdaptCont',
    ContResOut (..),
    _ContOut,
    _ResOut,
    ContRes (..),
    _Cont,
    _Res,
    step,
    unlessP,
    liftP_,
    finishP,
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

type ProgramSt st i o m = Pair st (Program st i o m)

type ProgramCont st i o m s = (s -> Program st i o m) -> Program st i o m

type ProgramAdaptCont pi po st i o m s = (pi -> Maybe i) -> (o -> po) -> ProgramCont () pi po m s

type ProgramCont' st i o m = Program st i o m -> Program st i o m

type ProgramAdaptCont' pi po st i o m = (pi -> Maybe i) -> (o -> po) -> ProgramCont' () pi po m

data ContResOut st i o m
  = ContOut (Maybe o) (Pair st (Program st i o m))
  | ResOut (Pair st (Either Error ()))

$(makePrisms 'ResOut)

data ContRes st i o m
  = Cont (Pair st (Program st i o m))
  | Res (Pair st (Either Error ()))

$(makePrisms 'Res)

-- TODO: fix this show instance
instance (Show o) => Show (Program st i o m) where
  show (Lift _ _) = "Lift"
  show (GetState _) = "GetState"
  show (PutState _ _) = "PutState"
  show (ModifyState _ _) = "ModifyState"
  show (WaitInput _) = "WaitInput"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish"
  show (Pipe _ _) = "Pipe"

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

unlessP :: Bool -> (a -> a) -> a -> a
unlessP True _ cont = cont
unlessP False act cont = act (cont)
{-# INLINE unlessP #-}

liftP_ :: m a -> Program st i o m -> Program st i o m
liftP_ act cont = Lift act $ \_ -> cont
{-# INLINE liftP_ #-}

finishP :: Program st i o m
finishP = Finish (Right ())
{-# INLINE finishP #-}
