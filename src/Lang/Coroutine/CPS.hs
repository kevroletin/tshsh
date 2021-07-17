{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- An implementation of coroutines. It's features are:
-- + simple implementation (implementation with an interpreter optimization
--   is under 100loc
module Lang.Coroutine.CPS
  ( Program (..),
    Program',
    ProgramCont,
    ProgramCont',
    ResO (..),
    _ContO,
    _ResO,
    Res (..),
    _Cont,
    _Res,
    step,
  )
where

import Control.Lens
import Data.Profunctor
import Protolude
import Prelude (Show (..))

type Error = Text

-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- s - input type for Lam-bdas; used mostly to do (andThen (andThen a b) c) ==
--     (andThen a (andThen b c)) optimization
data Program i o s t where
  WaitInput :: (i -> Program i o () t) -> Program i o () t
  Lam :: (s -> Program i o () t) -> Program i o s t
  Output :: o -> Program i o () t -> Program i o () t
  Finish :: Either Error t -> Program i o () t

type Program' i o t = Program i o () t

type ProgramCont i o s = forall t. (s -> Program i o () t) -> Program i o () t

type ProgramCont' i o = forall t. Program i o () t -> Program i o () t

data ResO i o r
  = ContO (Maybe o) (Program i o () r)
  | ResO (Either Error r)

$(makePrisms 'ResO)

data Res i o r
  = Cont (Program i o () r)
  | Res (Either Error r)

$(makePrisms 'Res)

-- TODO: fix this show instance
instance (Show o) => Show (Program i o a r) where
  show (WaitInput _) = "WaitInput"
  show (Lam _) = "Lam"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish "

deriving instance (Show i, Show o, Show r) => Show (ResO i o r)

deriving instance (Show i, Show o, Show r) => Show (Res i o r)

step :: forall i o a r. Maybe i -> a -> Program i o a r -> ResO i o r
step (Just i) _ (WaitInput cont) = step Nothing () (cont i)
step Nothing _ x@(WaitInput _) = ContO Nothing x
step i a (Lam val) = step i () (val a)
step Nothing _ (Output x next) = ContO (Just x) next
step (Just _) _ (Output _ _) = panic "Consume all the outputs first"
step Nothing _ (Finish a) = ResO a
step (Just _) _ (Finish _) = panic "Consume all the outputs before reading a result"
