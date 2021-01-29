module Control.ShiftMap where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.RWS.Trans (RWSResult(..), RWST(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- Say you have a function `f` that accepts a value `m a`.
-- Now you pass it a value `m b` instead. We need to figure
-- out the various ways in which we can still get the function to
-- work, by either constraining things, or by passing extra information.
--
-- Let's consider functions which combine the passed in value with other
-- values in some way. For example, if it's a semigroup, then it can
-- append the `m a` with other `m a`s.
--
-- Now that we actually passed in a `m b`, it's not possible to combine it.
-- But, what if we have a way to convert a `m a` into an `m b`. Then the
-- function can just convert its internal `m a` into an `m b` and combine them.
-- For this, it's enough to have an `a -> b` transformation and `map` it
-- over the `m a`.
--
-- This class of functions is captured by the `shiftMap` class.
--
-- For Concur, the UIs are monoidal, and shiftMap is used to apply
-- UI transformations, as long as they only do monoidal operations,
-- to things other than raw widgets.

-- | Avoiding monad-control for as long as possible
class ShiftMap s t where
  shiftMap :: forall a. (forall b. (a -> b) -> s b -> s b) -> t a -> t a

-- Instances for common transformers
-- It's not possible to use the `map*` functions anymore

instance exceptShiftMap :: ShiftMap m (ExceptT e m) where
  shiftMap f (ExceptT m) = ExceptT do f Right m

instance rwsShiftMap :: Monoid w => ShiftMap m (RWST r w s m) where
  shiftMap f (RWST g) = RWST \r s -> f (\a -> RWSResult s a mempty) (g r s)

instance readerShiftMap :: ShiftMap m (ReaderT r m) where
  shiftMap f (ReaderT m) = ReaderT \r -> f identity (m r)

instance stateShiftMap :: Monad m => ShiftMap m (StateT s m) where
  shiftMap f (StateT g) = StateT \s -> f (\a -> Tuple a s) (g s)

instance writerShiftMap :: Monoid w => ShiftMap m (WriterT w m) where
  shiftMap f (WriterT m) = WriterT do f (\a -> Tuple a mempty) m
