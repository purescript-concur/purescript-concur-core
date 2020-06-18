module Concur.Core.Types where

import Prelude

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
import Data.FoldableWithIndex (foldMapWithIndex, foldrWithIndex)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)

type WithHandler b a = ((a -> Effect Unit)) -> Effect b

mapWithHandler :: forall a b c. (a -> b) -> WithHandler c a -> WithHandler c b
mapWithHandler f g = \cb -> g (cb <<< f)

data WidgetStep v a
  = WidgetStepEff (Effect a)
  | WidgetStepView (WithHandler v a)
  -- TODO
  -- | WidgetStepViewStuck v
  | WidgetStepStuck

-- derive instance widgetStepFunctor :: Functor (WidgetStep v)
instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStepEff e) = WidgetStepEff (map f e)
  map f (WidgetStepView v) = WidgetStepView $ mapWithHandler f v
  map _ WidgetStepStuck = WidgetStepStuck

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

          WidgetStepView o -> combineInner (NEA.singleton o) x.tail
          WidgetStepStuck -> unWidget (orr x.tail)

    combineInner ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (WithHandler v' (Free (WidgetStep v') a)) ->
      Array (Widget v' a) ->
      Free (WidgetStep v') a
    combineInner vs freeArr = case A.uncons freeArr of
      -- We have collected all the inner conts
      Nothing -> combineConts vs
      Just x -> case resume (unWidget x.head) of
        Right a -> pure a
        Left xx -> case xx of
          WidgetStepEff eff -> wrap $ WidgetStepEff do
            w <- eff
            pure $ combineInner vs $ A.cons (Widget w) x.tail
          WidgetStepView c -> combineInner (NEA.snoc vs c) x.tail
          WidgetStepStuck -> combineInner vs x.tail

    combineConts ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (WithHandler v' (Free (WidgetStep v') a)) ->
      Free (WidgetStep v') a
    combineConts ws = wrap $ WidgetStepView $ merge ws

    merge ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (WithHandler v' (Free (WidgetStep v') a)) ->
      WithHandler v' (Free (WidgetStep v') a)
    merge ws = mapWithHandler (\nea -> combine (map Widget nea)) $ mergeWithHandlers (wrap <<< WidgetStepView) ws


mergeWithHandlers
  :: forall v a
   . Monoid v
  => (WithHandler v a -> a)
  -> NonEmptyArray (WithHandler v a)
  -> WithHandler v (NEA.NonEmptyArray a)
mergeWithHandlers mkh vs = \cb ->
  let mkCb i = \val -> cb (fromMaybe vs' (NEA.updateAt i val vs'))
  in foldMapWithIndex (\i f -> f (mkCb i)) vs
  where vs' = map mkh vs

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
mapViewStep f (WidgetStepView v) = WidgetStepView (map f <$> v)
mapViewStep f WidgetStepStuck = WidgetStepStuck

stuck :: forall v a. Widget v a
stuck = Widget $ liftF WidgetStepStuck

display :: forall v a. v -> Widget v a
-- TODO: Instead of carrying around a callback which will never be called, use a special constructor WidgetStepViewStuck
display v = Widget $ wrap $ WidgetStepView \cb -> pure v

-- Sync eff
effAction ::
  forall a v.
  Effect a ->
  Widget v a
effAction = Widget <<< liftF <<< WidgetStepEff

-- Async aff
affAction ::
  forall a v.
  WithHandler v a ->
  Widget v a
affAction = Widget <<< liftF <<< WidgetStepView

-- Async callback
-- asyncAction
--   :: forall v a
--   .  ((a -> Effect Unit) -> Effect (Effect Unit))
--   -> Widget v a
-- asyncAction handler = affAction (Observer handler)

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

-- instance widgetMonadObserver :: (Monoid v) => MonadObserver (Widget v) where
--   liftObserver = affAction mempty
    -- Widget $ liftF $ WidgetStep $ Right { view: mempty, cont: aff }

mkNodeWidget :: forall v a. ((Free (WidgetStep v) a -> Effect Unit) -> v -> v) -> Widget v a -> Widget v a
mkNodeWidget f (Widget w) = case resume w of
  Right _ -> Widget w
  Left x -> case x of
    WidgetStepStuck -> Widget w
    WidgetStepEff eff -> Widget $ wrap $ WidgetStepEff do
      w' <- eff
      pure $ unWidget $ mkNodeWidget f $ Widget w'
    WidgetStepView g -> Widget $ wrap $ WidgetStepView \cb -> f cb <$> g cb

mkLeafWidget :: forall v a. ((Free (WidgetStep v) a -> Effect Unit) -> v) -> Widget v a
mkLeafWidget = Widget <<< wrap <<< WidgetStepView <<< adapter
  where
  adapter h cb = pure (h cb)
