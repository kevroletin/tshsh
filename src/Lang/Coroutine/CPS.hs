{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- An implementation of coroutines. It's features are:
-- + simple implementation under 100loc
-- + interprets to a monadic value, provides Lift operation
-- + built-in state
module Lang.Coroutine.CPS
  ( Program (..),
    ProgramCont,
    ProgramCont',
    ContResOut (..),
    _ContOut,
    _ResOut,
    ContRes (..),
    _Cont,
    _Res,
    step,
  )
where

import Control.Lens
import Data.Strict.Tuple
import Protolude
import Prelude (Show (..))

type Error = Text

-- st - state (for GetState, Put state)
-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- m - base Monad for Lift (and for the interpreter 'step')
data Program st i o m t where
  Lift :: m a -> (a -> Program st i o m t) -> Program st i o m t
  GetState :: (st -> Program st i o m t) -> Program st i o m t
  PutState :: st -> Program st i o m t -> Program st i o m t
  WaitInput :: (i -> Program st i o m t) -> Program st i o m t
  Output :: o -> Program st i o m t -> Program st i o m t
  Finish :: Either Error t -> Program st i o m t

type ProgramCont st i o m s = forall t. (s -> Program st i o m t) -> Program st i o m t

type ProgramCont' st i o m = forall t. Program st i o m t -> Program st i o m t

data ContResOut st i o m r
  = ContOut (Maybe o) (Pair st (Program st i o m r))
  | ResOut (Pair st (Either Error r))

$(makePrisms 'ResOut)

data ContRes st i o m r
  = Cont (Pair st (Program st i o m r))
  | Res (Pair st (Either Error r))

$(makePrisms 'Res)

-- TODO: fix this show instance
instance (Show o) => Show (Program st i o m r) where
  show (Lift _ _) = "Lift"
  show (GetState _) = "GetState"
  show (PutState _ _) = "PutState"
  show (WaitInput _) = "WaitInput"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish "

deriving instance (Show st, Show i, Show o, Show r) => Show (ContResOut st i o m r)

deriving instance (Show st, Show i, Show o, Show r) => Show (ContRes st i o m r)

step ::
  forall st i o m r.
  Monad m =>
  Maybe i ->
  Pair st (Program st i o m r) ->
  m (ContResOut st i o m r)
step i (st :!: Lift ma cont) = do
  a <- ma
  step i (st :!: cont a)
step i (st :!: GetState cont) = step i (st :!: cont st)
step i (_ :!: PutState st cont) = step i (st :!: cont)
step (Just i) (st :!: WaitInput cont) = step Nothing (st :!: cont i)
step Nothing x@(_ :!: WaitInput _) = pure $ ContOut Nothing x
step Nothing (st :!: Output x next) = pure $ ContOut (Just x) (st :!: next)
step (Just _) (_st :!: Output _ _) = panic "Consume all the outputs first"
step Nothing (st :!: Finish a) = pure $ ResOut (st :!: a)
step (Just _) (_st :!: Finish _) = panic "Consume all the outputs before reading a result"
