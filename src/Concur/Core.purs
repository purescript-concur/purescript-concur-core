module Concur.Core
( module Concur.Core.Types
, module Concur.Core
, module Concur.Core.LiftWidget
, module Concur.Core.IsWidget
)
where

import Prelude (Unit, discard, pure, ($))
import Concur.Core.IsWidget (class IsWidget)
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Types (Widget(..), mkWidget, runWidget, unWid, Result(..))
import Effect (Effect)

-- Helpers for some very common use of unsafe blocking io

-- | Construct a widget, by wrapping an existing widget in a view event
mkNodeWidget ::
  forall a v. ((a -> Effect Unit) -> v -> v) ->
  Widget v a ->
  Widget v a
mkNodeWidget mkView w = mkWidget \cb -> do
  runWidget w (f cb)
  where
    f cb = \x -> case x of
      View vc -> cb (View $ vp vc cb)
      Partial a -> cb (Partial a)
      Completed a -> cb (Completed a)
    vp vc cb = mkView (\a -> cb (Partial a)) vc

-- | Construct a widget with just props
mkLeafWidget ::
  forall a v.
  ((a -> Effect Unit) -> v) ->
  Widget v a
mkLeafWidget mkView = mkWidget \cb -> do
  cb (View $ v cb)
  pure (pure (unWid (mkLeafWidget mkView)))
  where
    v cb = mkView (\a -> cb (Partial a))
