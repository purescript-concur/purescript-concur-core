module Concur.Core.Discharge where

import Prelude

import Concur.Core.Types (Widget(..), WidgetStep(..), unWidget)
import Control.Monad.Free (resume, runFreeM, wrap)
import Control.Monad.Writer (runWriterT, tell)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

-- Widget discharge strategies
-- | Discharge a widget.
-- | 1. Runs the Effect action
-- | 2. Forks the Aff action
-- | 3. Extracts and returns the view
discharge ::
  forall a v.
  Monoid v =>
  (Either Error (Widget v a) -> Effect Unit) ->
  Widget v a ->
  Effect v
discharge handler (Widget w) = case resume w of
  Right _ -> pure mempty
  Left (WidgetStepEff eff) -> do
      w' <- eff
      discharge handler (Widget w')
  Left (WidgetStepView ws) -> do
      runAff_ (handler <<< map Widget) ws.cont
      pure ws.view

-- | Discharge only the top level blocking effect of a widget (if any) to get access to the view
-- | Returns the view, and the remaining widget
dischargePartialEffect ::
  forall a v.
  Monoid v =>
  Widget v a ->
  Effect (Tuple (Widget v a) v)
dischargePartialEffect w = case resume (unWidget w) of
  Right _ -> pure (Tuple w mempty)
  Left (WidgetStepEff eff) -> do
      w' <- eff
      dischargePartialEffect (Widget w')
  Left (WidgetStepView ws) -> pure (Tuple (Widget (wrap (WidgetStepView ws))) ws.view)

-- | Dischage all effects and recieve the result and viewss as Array.
-- | Mainly for testing.
-- | Be carefull that never ending Widget will convert to never ending Aff.
dischargeAll :: forall v a. Widget v a -> Aff { result :: a, views :: Array v }
dischargeAll widget = do
  Tuple result views <- runWriterT $ runFreeM interpret (unWidget widget)
  pure { result, views }
  where
    interpret (WidgetStepEff eff) =
      liftEffect eff
    interpret (WidgetStepView rec) = do
      tell $ singleton rec.view
      liftAff rec.cont
{-
-- | Discharge a widget, forces async resolution of the continuation.
-- | 1. Runs the Effect action
-- | 2. Forks the Aff action, using an async delay to guarantee that handler will not be called synchronously.
-- | 3. Extracts and returns the view
dischargeAsync ::
  forall a v.
  Monoid v =>
  (Either Error (Widget v a) -> Effect Unit) ->
  Widget v a ->
  Effect v
dischargeAsync handler (Widget w) = case resume w of
  Right _ -> pure mempty
  Left (WidgetStep mws) -> do
    ews <- mws
    case ews of
      Left w' -> dischargeAsync handler (Widget w')
      Right ws -> do
        runAff_ (handler <<< map Widget) do
          delay (Milliseconds 0.0)
          a <- ws.cont
          pure a
        pure ws.view

-- | Discharge a sync widget.
-- | 1. Runs the Effect action
-- | 2. Tries to run the Aff action without forking
-- |    If it succeeds, then it returns (Left <result>)
-- |    If it cannot be done, then it returns (Right <remaining Effect action>)
-- | 3. Extracts and returns the view
dischargeSync ::
  forall a v.
  Monoid v =>
  (Either Error (Widget v a) -> Effect Unit) ->
  Widget v a ->
  Effect v
dischargeSync handler (Widget winit) = go winit
  where
  go w = case resume w of
    Right _ -> pure mempty
    Left (WidgetStep mws) -> do
      ews <- mws
      case ews of
        Left w' -> dischargeSync handler (Widget w')
        Right ws -> do
          res <- ioToIosync ws.cont
          case res of
            Left w' -> go w'
            Right io -> do
              runAff_ (handler <<< map Widget) io
              pure ws.view

-- UTIL
-- Potentially early resolve an Aff (if it can be run synchronously)
-- if so, return (Left result), else return (Right Aff)
ioToIosync ::
  forall a.
  Aff a ->
  Effect (Either a (Aff a))
ioToIosync io = do
  v <- EVar.empty
  _ <- launchAff do
    a <- io
    liftAff (AVar.put a v)
  ma <- (EVar.tryTake v)
  case ma of
    Nothing -> pure (Right (liftAff (AVar.take v)))
    Just a -> pure (Left a)
-}
