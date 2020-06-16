module Concur.Core.Types where

import Prelude

import Concur.Core.Event (Observer(..), parIndex)
import Control.Alternative (class Alternative)
import Control.Monad.Free (Free, hoistFree, liftF, resume, wrap)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Plus (class Alt, class Plus, alt, empty)
import Control.ShiftMap (class ShiftMap)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)

data WidgetStep v a
  = WidgetStepEff (Effect a)
  | WidgetStepCont (Observer a)
  -- The expectation is that these views will never be sequenced in a row
  -- However, if they are sequenced in a row, only the last view will take effect
  -- TODO: Perhaps the views should be concatenated??
  | WidgetStepView v a
  -- This modifies all views inside it
  | WidgetStepMapView (v -> v) a
  -- TODO: This modifies all views inside it, but can also add handlers
  -- TODO: | WidgetStepNestedView ((b -> Effect Unit) -> v -> v) a
  | WidgetStepHalt

-- derive instance widgetStepFunctor :: Functor (WidgetStep v)
instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStepEff e) = WidgetStepEff (map f e)
  map f (WidgetStepView v a) = WidgetStepView v (f a)
  map f (WidgetStepMapView g a) = WidgetStepMapView g (f a)
  map f (WidgetStepCont o) = WidgetStepCont (map f o)
  map _ WidgetStepHalt = WidgetStepHalt

newtype Widget v a
  = Widget (Free (WidgetStep v) a)

unWidget :: forall v a. Widget v a -> Free (WidgetStep v) a
unWidget (Widget w) = w

derive newtype instance widgetFunctor :: Functor (Widget v)

derive newtype instance widgetBind :: Bind (Widget v)

derive newtype instance widgetApplicative :: Applicative (Widget v)

derive newtype instance widgetApply :: Apply (Widget v)

instance widgetMonad :: Monad (Widget v)

derive newtype instance widgetMonadRec :: MonadRec (Widget v)

instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

instance widgetMultiAlternative ::
  ( Monoid v
  ) =>
  MultiAlternative (Widget v) where
  orr wss = case NEA.fromArray wss of
    Just wsne -> Widget $ combine wsne
    Nothing -> empty
    where

    combine ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Widget v' a) ->
      Free (WidgetStep v') a
    combine wfs =
      let x = NEA.uncons wfs
      in case resume (unWidget x.head) of
        Right a -> pure a
        -- TODO: This wrap probably cannot be wished away
        Left xx -> case xx of
          WidgetStepEff eff -> wrap $ WidgetStepEff do
            w <- eff
            pure $ combine $ NEA.cons' (Widget w) x.tail

          -- TODO: Instead of using wrap here, maybe collapse views
          --       This may be important for performance
          WidgetStepView v w -> wrap $ WidgetStepView v $ combine $ NEA.cons' (Widget w) x.tail
          WidgetStepMapView f w -> wrap $ WidgetStepMapView f $ combine $ NEA.cons' (Widget w) x.tail
          WidgetStepCont o -> combineInner (NEA.singleton o) x.tail
          WidgetStepHalt -> unWidget (orr x.tail)

    combineInner ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Observer (Free (WidgetStep v') a)) ->
      Array (Widget v' a) ->
      Free (WidgetStep v') a
    combineInner ws freeArr = case NEA.fromArray freeArr of
      -- We have collected all the inner conts
      Nothing -> combineConts ws --wrap $ WidgetStep $ Right wsr
      Just freeNarr -> combineInner1 ws freeNarr

    combineInner1 ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Observer (Free (WidgetStep v') a)) ->
      NonEmptyArray (Widget v' a) ->
      Free (WidgetStep v') a
    combineInner1 ws freeNarr =
      let x = NEA.uncons freeNarr
      in case resume (unWidget x.head) of
        Right a -> pure a
        Left (WidgetStepEff eff) -> wrap $ WidgetStepEff do
            w <- eff
            pure $ combineInner1 ws $ NEA.cons' (Widget w) x.tail
        Left (WidgetStepView v w) -> wrap $ WidgetStepView v $ combineInner1 ws (NEA.cons' (Widget w) x.tail)
        Left (WidgetStepMapView f w) -> wrap $ WidgetStepMapView f $ combineInner1 ws (NEA.cons' (Widget w) x.tail)
        Left (WidgetStepCont c) -> combineInner (NEA.snoc ws c) x.tail
        Left WidgetStepHalt -> combineInner ws x.tail

    combineConts ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Observer (Free (WidgetStep v') a)) ->
      Free (WidgetStep v') a
    combineConts ws = wrap $ WidgetStepCont $ merge ws

    merge ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Observer (Free (WidgetStep v') a)) ->
      Observer (Free (WidgetStep v') a)
    merge ws = map func obs
      where
      wsm = map (Widget <<< wrap <<< WidgetStepCont) ws

      -- TODO: We know the array is non-empty. We need something like foldl1WithIndex.
      -- TODO: All the Observer in ws is already discharged. Use a more efficient way than combine to process it
      -- TODO: Also, more importantly, we would like to not have to cancel running fibers unless one of them returns a result
      -- MAP OVER OBSERVER. SEE IF WE CAN OPTIMISE THIS (COYONEDA).
      obs = parIndex (NEA.toArray ws)
      func {i, val:e} = combine (fromMaybe wsm (NEA.updateAt i (Widget e) wsm))


-- | Run multiple widgets in parallel until *all* finish, and collect their outputs
-- | Contrast with `orr`
-- TODO: Performance? Don't orr with `empty`.
andd ::
  forall v a.
  Monoid v =>
  Array (Widget v a) ->
  Widget v (Array a)
andd ws = do
  Tuple i e <- foldrWithIndex (\i w r -> alt (map (Tuple i) w) r) empty ws
  let ws' = fromMaybe ws $ A.deleteAt i ws
  if A.length ws' <= 0
    then pure [e]
    else do
      rest <- andd ws'
      pure $ fromMaybe [] $ A.insertAt i e rest

instance widgetSemigroup :: (Monoid v) => Semigroup (Widget v a) where
  append w1 w2 = orr [w1, w2]

instance widgetMonoid :: (Monoid v) => Monoid (Widget v a) where
  mempty = empty

instance widgetAlt :: (Monoid v) => Alt (Widget v) where
  alt = append

instance widgetPlus :: (Monoid v) => Plus (Widget v) where
  empty = display mempty

instance widgetAlternative :: (Monoid v) => Alternative (Widget v)

-- Pause for a negligible amount of time. Forces continuations to pass through the trampoline.
-- (Somewhat similar to calling `setTimeout` of zero in Javascript)
-- Avoids stack overflows in (pathological) cases where a widget calls itself repeatedly without any intervening widgets or effects.
-- E.g. -
--   BAD  `counter n = if n < 10000 then counter (n+1) else pure n`
--   GOOD `counter n = if n < 10000 then (do pulse; counter (n+1)) else pure n`
pulse ::
  forall v.
  Monoid v =>
  Widget v Unit
pulse = effAction (pure unit)

mapView :: forall a v. (v -> v) -> Widget v a -> Widget v a
mapView f (Widget w) = Widget (hoistFree (mapViewStep f) w)

mapViewStep :: forall v a. (v -> v) -> WidgetStep v a -> WidgetStep v a
mapViewStep f (WidgetStepEff e) = WidgetStepEff e
mapViewStep f (WidgetStepCont c) = WidgetStepCont c
mapViewStep f (WidgetStepView v a) = WidgetStepView (f v) a
mapViewStep f (WidgetStepMapView g a) = WidgetStepMapView (f <<< g) a
mapViewStep f WidgetStepHalt = WidgetStepHalt

halt :: forall v a. Widget v a
halt = Widget $ liftF WidgetStepHalt

display :: forall v a. v -> Widget v a
display v = Widget $ wrap $ WidgetStepView v $ unWidget halt

-- Sync eff
effAction ::
  forall a v.
  Effect a ->
  Widget v a
effAction = Widget <<< liftF <<< WidgetStepEff

-- Async aff
affAction ::
  forall a v.
  Observer a ->
  Widget v a
affAction = Widget <<< liftF <<< WidgetStepCont

-- Async callback
asyncAction
  :: forall v a
  .  ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Widget v a
asyncAction handler = affAction (Observer handler)

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

-- instance widgetMonadObserver :: (Monoid v) => MonadObserver (Widget v) where
--   liftObserver = affAction mempty
    -- Widget $ liftF $ WidgetStep $ Right { view: mempty, cont: aff }
