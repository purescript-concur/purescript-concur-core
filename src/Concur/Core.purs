module Concur.Core
( module Concur.Core.Types
, module Concur.Core
, module Concur.Core.LiftWidget
, module Concur.Core.IsWidget
)
where

import Concur.Core.IsWidget (class IsWidget)
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Types (Widget(..), WidgetStep(..), unWidget)
import Control.Monad.Free (Free, wrap, resume)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (alt)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.AVar (empty, tryPut) as EVar
import Effect.Aff.AVar (take) as AVar
import Effect.Aff.Class (liftAff)
import Prelude (Unit, bind, map, pure, void, ($))

-- Helpers for some very common use of unsafe blocking io

-- | Construct a widget, by wrapping an existing widget in a view event
mkNodeWidget ::
  forall a v.
  ((a -> Effect Unit) -> v -> v) ->
  Widget v a ->
  Widget v a
mkNodeWidget mkView (Widget w) = Widget (mkNodeWidget' mkView w)

-- Private
mkNodeWidget' :: forall a v. ((a -> Effect Unit) -> v -> v) -> Free (WidgetStep v) a -> Free (WidgetStep v) a
mkNodeWidget' mkView w = case resume w of
  Right a -> pure a
  Left (WidgetStepEff eff) -> wrap $ WidgetStepEff do
      w' <- eff
      pure $ mkNodeWidget' mkView w'
  Left (WidgetStepView wsr) -> wrap $ WidgetStepEff do
      var <- EVar.empty
      let eventHandler = (\a -> void (EVar.tryPut (pure a) var))
      let cont' = sequential (alt (parallel (liftAff (AVar.take var)))
                                  (parallel (map (mkNodeWidget' mkView) wsr.cont))
                             )
      pure $ wrap $ WidgetStepView
        { view: mkView eventHandler wsr.view
        , cont: cont'
        }
-- | Construct a widget with just props
mkLeafWidget ::
  forall a v.
  ((a -> Effect Unit) -> v) ->
  Widget v a
mkLeafWidget mkView = Widget $ wrap $ WidgetStepEff do
  var <- EVar.empty
  pure $ wrap $ WidgetStepView
    { view: mkView (\a -> void (EVar.tryPut (pure a) var))
    , cont: liftAff (AVar.take var)
    }
