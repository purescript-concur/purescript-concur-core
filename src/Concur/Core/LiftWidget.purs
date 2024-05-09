module Concur.Core.LiftWidget where

import Concur.Core.Types (Widget)
import Control.Monad (class Monad)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Function (identity, (<<<))

class LiftWidget v m where
  liftWidget :: forall a. Widget v a -> m a

instance LiftWidget v (Widget v) where
  liftWidget = identity
else instance (Monad m, MonadTrans t, LiftWidget v m) => LiftWidget v (t m) where
  liftWidget = lift <<< liftWidget

