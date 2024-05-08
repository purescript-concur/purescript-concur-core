module Control.ShiftMap where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.RWS.Trans (RWSResult(..), RWST(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- | Avoiding monad-control for as long as possible
-- | (convert functions on widgets to functions on transformer stacks
-- | wrapping widgets).
class ShiftMap s t where
  shiftMap :: forall a. (forall b. (a -> b) -> s b -> s b) -> t a -> t a

-- Instances for common transformers
-- It's not possible to use the `map*` functions anymore

instance ShiftMap m (ExceptT e m) where
  shiftMap f (ExceptT m) = ExceptT do f Right m

instance Monoid w => ShiftMap m (RWST r w s m) where
  shiftMap f (RWST g) = RWST \r s -> f (\a -> RWSResult s a mempty) (g r s)

instance ShiftMap m (ReaderT r m) where
  shiftMap f (ReaderT m) = ReaderT \r -> f identity (m r)

instance Monad m => ShiftMap m (StateT s m) where
  shiftMap f (StateT g) = StateT \s -> f (\a -> Tuple a s) (g s)

instance Monoid w => ShiftMap m (WriterT w m) where
  shiftMap f (WriterT m) = WriterT do f (\a -> Tuple a mempty) m

