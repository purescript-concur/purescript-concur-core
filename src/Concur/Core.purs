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

-- Helpers for some very common use of unsafe blocking io

-- | Construct a widget, by wrapping an existing widget in a view event
mkNodeWidget
  :: forall a v
   . ((a -> Effect Unit) -> v -> v)
  -> Widget v a
  -> Widget v a
mkNodeWidget mkView w = mkWidget \cb -> do
  runWidget w (f cb)
  where
  f cb = \x -> case x of
    View vc -> cb (View $ vp vc cb)
    Completed a -> cb (Completed a)
  vp vc cb = mkView (\a -> cb (Completed a)) vc

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
