{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An implementation of coroutines. It's features
-- AndThen combinator which allows using do-notation to combine programs.
--
-- We've optimized AndThen so that both constructing programs (similar to
-- monodic >>=) and evaluating a single node should have O(1) amortized
-- complexity (worst case is O(n), but consecutive calls will be O(1)).
--
-- Functor, Applicative and Monad laws aren't strictly satisfied. But
-- combining programs using operations from mentioned typeclasses
-- results programs with identical behavior.
--
-- Using pseudo code:
-- a =eval= b ==> forall inp. eval inp a == eval inp b
--
-- Functor laws:
-- @'fmap' 'id' =eval= 'id'@
-- @'fmap' (f . g) =eval= 'fmap' f . 'fmap' g@
--
-- Similarly for Applicative, Monad laws.
--
-- TODO: check monad Laws
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
module Lang.Coroutine.MonadT where

import Control.Lens
import Protolude
import Prelude (Show (..))

type Error = Text

-- i, o - input/output types for yield
-- t - returned value (when program is successfully finished)
-- s - input type for Lam-bdas; used mostly to do (andThen (andThen a b) c) ==
--     (andThen a (andThen b c)) optimization
data Program i o s (m :: * -> *) t where
  Lift :: m a -> Program i o () m a
  WaitInput :: Program i o () m i
  Lam :: (s -> Program i o () m t) -> Program i o s m t
  Output :: o -> Program i o () m ()
  Finish :: Either Error t -> Program i o () m t
  AndThen ::
    Program i o s m b ->
    Program i o b m t ->
    Program i o s m t

-- TODO: x this show instance
instance (Show o) => Show (Program i o a m r) where
  show (Lift _) = "Lift"
  show (WaitInput) = "WaitInput"
  show (Lam _) = "Lam"
  show (Output o) = "Output " <> Protolude.show o
  show (Finish _) = "Finish "
  show (AndThen a b) = "(AndThen " <> Protolude.show a <> " " <> Protolude.show b <> ")"

instance Functor (Program i o () m) where
  fmap f p = p `AndThen` Lam (Finish . Right . f)

instance Applicative (Program i o () m) where
  pure = Finish . Right
  f <*> x = f `AndThen` Lam (<$> x)

instance Monad (Program i o () m) where
  m >>= f = m `AndThen` Lam f

instance MonadIO m => MonadIO (Program i o () m) where
  liftIO = Lift . liftIO

instance MonadState s m => MonadState s (Program i o () m) where
  get = Lift get
  put s = Lift (put s)

-- instance MonadReader r m => MonadReader r (Program i o () m) where
--   ask = Lift ask
--   local f p = ... since we are computing everything inside of the 'step'
--     function, we need to make a new node to postpone
--     MonadReader r m => m a -> m a transformation until evaluation.
--     The strategy would be: step computes a value of m a, and then applies
--     local just before returning it.

type Program' i o m t = Program i o () m t

data ResO i o m r
  = ContO (Maybe o) (Program i o () m r)
  | ResO (Either Error r)
  deriving (Show)

$(makePrisms 'ResO)

data Res i o m r
  = Cont (Program i o () m r)
  | Res (Either Error r)
  deriving (Show)

$(makePrisms 'Res)

step :: forall i o a m r. Monad m => Maybe i -> a -> Program i o a m r -> m (ResO i o m r)
step (Just i) _ WaitInput = pure $ ResO (Right i)
step Nothing _ WaitInput  = pure $ ContO Nothing WaitInput
step i a (Lam val) = step i () (val a)
step Nothing _ (Output x) = pure $ ContO (Just x) (Finish (Right ()))
step (Just _) _ (Output _) = panic "Consume all the outputs first"
step Nothing _ (Finish a) = pure $ ResO a
step (Just _) _ (Finish _) = panic "Consume all the outputs before reading a result"
step i x (AndThen (AndThen a b) c) = step i x (AndThen a (AndThen b c))
step i a (AndThen val fb) =
  step i a val >>= \case
    ResO (Right b) -> step Nothing b fb
    ResO (Left err) -> pure $ ResO (Left err)
    ContO x cont -> pure $ ContO x (AndThen cont fb)
step Nothing _ (Lift m) = ContO Nothing . Finish . Right <$> m
step (Just _) _ (Lift _) = panic "executing Lift doesn't require any inputs"
