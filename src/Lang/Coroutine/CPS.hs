{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}

-- An implementation of coroutines. It's features are:
-- + simple implementation (implementation with an interpreter optimization
--   is under 100loc
module Lang.Coroutine.CPS
  -- ( Program st (..),
  --   Program',
  --   ProgramCont,
  --   ProgramCont',
  --   ResO (..),
  --   _ContO,
  --   _ResO,
  --   Res (..),
  --   _Cont,
  --   _Res,
  --   step,
  -- )
where

import Protolude

type Error = Text

-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- s - input type for Lam-bdas; used mostly to do (andThen (andThen a b) c) ==
--     (andThen a (andThen b c)) optimization
data Program st i o s t where
  GetState :: (st -> Program st i o () t) -> Program st i o () t
  PutState :: st -> Program st i o () t -> Program st i o () t
  WaitInput :: (i -> Program st i o () t) -> Program st i o () t
  Lam :: (s -> Program st i o () t) -> Program st i o s t
  Output :: o -> Program st i o () t -> Program st i o () t
  Finish :: Either Error t -> Program st i o () t

-- type Program' i o t = Program st i o () t

-- type ProgramCont i o s = forall t. (s -> Program st i o () t) -> Program st i o () t

-- type ProgramCont' i o = forall t. Program st i o () t -> Program st i o () t

data ResO st i o r
  = ContO (Maybe o) (st, Program st i o () r)
  | ResO (st, Either Error r)

-- $(makePrisms 'ResO)

-- data Res st i o r
--   = Cont (st, Program st i o () r)
--   | Res (st, Either Error r)

-- $(makePrisms 'Res)

-- -- TODO: fix this show instance
-- instance (Show o) => Show (Program st i o a r) where
--   show (WaitInput _) = "WaitInput"
--   show (Lam _) = "Lam"
--   show (Output o _) = "Output " <> Protolude.show o
--   show (Finish _) = "Finish "

-- deriving instance (Show i, Show o, Show r) => Show (ResO i o r)

-- deriving instance (Show i, Show o, Show r) => Show (Res i o r)

step :: forall st i o a r. Maybe i -> a -> (st, Program st i o a r) -> ResO st i o r

step i a (st, GetState cont) = step i a (st, cont st)
step i a (_, PutState st cont) = step i a (st, cont)

step (Just i) _ (st, WaitInput cont) = step Nothing () (st, cont i)
step Nothing _ x@(_, WaitInput _) = ContO Nothing x
step i a (st, Lam val) = step i () (st, val a)
step Nothing _ (st, Output x next) = ContO (Just x) (st, next)
step (Just _) _ (_st, Output _ _) = panic "Consume all the outputs first"
step Nothing _ (st, Finish a) = ResO (st, a)
step (Just _) _ (_st, Finish _) = panic "Consume all the outputs before reading a result"
