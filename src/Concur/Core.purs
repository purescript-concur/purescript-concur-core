module Concur.Core
( module Concur.Core.Types
, module Concur.Core
, module Concur.Core.LiftWidget
, module Concur.Core.IsWidget
)
where

import Concur.Core.Event (Observer(..), mkObserver, par)
import Concur.Core.IsWidget (class IsWidget)
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Types (Widget(..), WidgetStep(..), unWidget)
import Control.Category ((<<<))
import Control.Monad.Free (Free, resume, wrap)
import Data.Either (Either(..))
import Effect (Effect)
import Prelude (Unit, bind, pure, ($))

-- Helpers for some very common use of unsafe blocking io

-- TODO: THIS SHOULD NOT BE EXTRACTED OUT OF THE BACKEND CODE.
-- EXTRACTING THIS OUT OF THE BACKEND IS CAUSING A RIGID mkNodeWidget TO BE USED
-- THAT mkNodeWidget IF FORCING "spooky action at a distance". GET RID OF!

{-
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
  Left x -> case x of
    WidgetStepEff eff -> wrap $ WidgetStepEff do
      w' <- eff
      pure $ mkNodeWidget' mkView w'
    WidgetStepView v w' -> do
-}

  -- Left (WidgetStepCont (Observer f)) -> wrap $ WidgetStepCont $ Observer \cb ->
  --   f \w' -> cb (mkNodeWidgetInner cb w')

  -- At this point, we don't have a wrapping observer. Create one
  -- Left (WidgetStepView v a) -> wrap $ WidgetStepCont $ Observer \cb ->
  --   ob <- mkObserver
  --   pure $ wrap $ WidgetStepView
  --     { view: mkView (ob.push <<< pure) wsr.view
  --     , cont: par [ob.subscribe, wsr.cont]
  --     }

  -- where
    -- Special case, view wrapped inside callback
    -- mkNodeWidgetInner cb w' = case resume w' of
    --   Right a -> pure a
    --   Left (WidgetStepEff eff) -> wrap $ WidgetStepEff do
    --     w'' <- eff
    --     pure $ mkNodeWidgetInner cb w''
    --   Left (WidgetStepView v w'') -> wrap $ WidgetStepView (mkView cb v) (mkNodeWidget' mkView w'')

{-

-- | Construct a widget with just props
mkLeafWidget ::
  forall a v.
  ((a -> Effect Unit) -> v) ->
  Widget v a
mkLeafWidget mkView = Widget $ wrap $ WidgetStepEff do
  ob <- mkObserver
  pure $ wrap $ WidgetStepView
    { view: mkView (ob.push <<< pure)
    , cont: ob.subscribe
    }
-}
