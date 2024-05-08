module Concur.Core
  ( module Concur.Core.Types
  , module Concur.Core
  , module Concur.Core.LiftWidget
  ) where

import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Types (Widget(..), mkWidget, runWidget, unWid, Result(..))
import Data.Monoid (mempty)
import Effect (Effect)
import Prelude (Unit, discard, pure, ($))

-- | Construct a widget, by wrapping an existing widget in a view event
mkNodeWidget
  :: forall a v
   . ((a -> Effect Unit) -> v -> v)
  -> Widget v a
  -> Widget v a
mkNodeWidget mkView w = mkWidget \cb ->
  runWidget w $ case _ of
    View v -> cb (View $ mkView (\a -> cb (Completed a)) v)
    Completed a -> cb (Completed a)

-- | Construct a widget with just props
mkLeafWidget
  :: forall a v
   . ((a -> Effect Unit) -> v)
  -> Widget v a
mkLeafWidget mkView = mkWidget \cb -> do
  cb $ View $ v cb
  pure mempty
  where
  v cb = mkView (\a -> cb (Completed a))

