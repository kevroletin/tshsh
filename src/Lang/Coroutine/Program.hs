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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Lang.Coroutine.Program where

import Prelude (Show(..))
import Protolude

-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- s - input type for Lam-bdas; used mostly to do (andThen (andThen a b) c) ==
--     (andThen a (andThen b c)) optimization
data Program i o s t where
  WaitInput :: (i -> Program i o () t) -> Program i o () t
  Lam :: (s -> Program i o () t) -> Program i o s t
  Output :: o -> Program i o () t -> Program i o () t
  Finish :: t -> Program i o () t
  AndThen :: Program i o s b
             -> Program i o b t
             -> Program i o s t

type Program' i o t = Program i o () t

data Res i o r = Cont (Maybe o) (Program i o () r)
               | Res r

-- TODO: fix this show instance
instance (Show o) => Show (Program i o a r) where
  show (WaitInput _) = "WaitInput"
  show (Lam _) = "Lam"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish "
  show (AndThen a b) = "(AndThen " <> Protolude.show a <> " " <> Protolude.show b <> ")"

deriving instance (Show i, Show o, Show r) => Show (Res i o r)

step :: forall i o a r . Maybe i -> a -> Program i o a r -> Res i o r
step (Just i) _ (WaitInput cont) = step Nothing () (cont i)
step Nothing _ x@(WaitInput _) = Cont Nothing x

step i a (Lam val) = step i () (val a)

step Nothing _ (Output x next) = Cont (Just x) next
step (Just _) _ (Output _ _) = panic "Consume all the outputs first"

step Nothing _ (Finish a) = Res a
step (Just _) _ (Finish _) = panic "Consume all the outputs before reading a result"

step i x (AndThen (AndThen a b) c) = step i x (AndThen a (AndThen b c))
step i a (AndThen val fb) =
    case step i a val of
      Res b -> step Nothing b fb
      Cont x cont -> Cont x (AndThen cont fb)
