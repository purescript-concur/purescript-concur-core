module Concur.Core.Types where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Free (Free, hoistFree, liftF, resume, wrap)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Alt, class Plus, alt, empty)
import Control.ShiftMap (class ShiftMap)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex, foldlWithIndex)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Semigroup.Foldable (foldMap1)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (empty, tryPut, tryTake) as EVar
import Effect.Aff (Aff, effectCanceler, makeAff, never, runAff_)
import Effect.Aff.AVar (take) as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Exception (Error)

type WidgetStepRecord v a
  = {view :: v, cont :: Aff a}

data WidgetStep v a
  = WidgetStepEff (Effect a)
  | WidgetStepView (WidgetStepRecord v a)

-- unWidgetStep ::
--   forall v a.
--   WidgetStep v a ->
--   Either (Effect a) (WidgetStepRecord v a)
-- unWidgetStep (WidgetStep x) = x

-- derive instance widgetStepFunctor :: Functor (WidgetStep v)
instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStepEff e) = WidgetStepEff (map f e)
  map f (WidgetStepView w) = WidgetStepView (w { cont = map f w.cont })

displayStep :: forall a v. v -> WidgetStep v a
displayStep v = WidgetStepView { view: v, cont: never }

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

-- Util
flipEither ::
  forall a b.
  Either a b ->
  Either b a
flipEither (Left a) = Right a
flipEither (Right b) = Left b

instance widgetMultiAlternative ::
  ( Monoid v
  ) =>
  MultiAlternative (Widget v) where
  orr wss = case NEA.fromArray wss of
    Just wsne -> Widget $ combine $ map unWidget wsne
    Nothing -> empty
    where
    combine ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Free (WidgetStep v') a) ->
      Free (WidgetStep v') a
    combine ws = wrap $ WidgetStepEff do
      res <- widgetStepRecords ws
      pure $ case res of
        Right a -> pure a
        Left wsrs -> wrap $ WidgetStepView
            { view: foldMap1 _.view wsrs,
             cont: merge wsrs (map _.cont wsrs)
            }

    toWidgetStepRec ::
      forall v' a.
      Free (WidgetStep v') a ->
      Effect (Either (WidgetStepRecord v' (Free (WidgetStep v') a)) a)
    toWidgetStepRec widget = go widget
      where
        go w = case (resume w) of
          Right a ->  pure $ Right  a
          Left (WidgetStepEff eff) -> do
            w' <- eff
            go w'
          Left (WidgetStepView wsr') -> pure $ Left wsr'

    widgetStepRecords ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Free (WidgetStep v') a) ->
      Effect (Either (NonEmptyArray (WidgetStepRecord v' (Free (WidgetStep v') a))) a)
    widgetStepRecords widgets =  tailRecM go { widgets: NEA.toArray widgets, wsrs: [] }
      where
        go rec = case (A.head rec.widgets) of
          Just y -> do
            etr <- toWidgetStepRec y
            pure case etr of
              Right a -> Done $ Right a
              Left wsr -> Loop { widgets: next rec, wsrs: (A.snoc rec.wsrs  wsr)}
          Nothing -> pure $ Done (Left $ fromMaybe fallback $ NEA.fromArray rec.wsrs)
        next rec = fromMaybe [] (A.tail rec.widgets)
        fallback = NEA.singleton { view: mempty, cont: never }

    merge ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (WidgetStepRecord v' (Free (WidgetStep v') a)) ->
      NonEmptyArray (Aff (Free (WidgetStep v') a)) ->
      Aff (Free (WidgetStep v') a)
    merge ws wscs = do
      let wsm = map (wrap <<< WidgetStepView) ws
      -- TODO: We know the array is non-empty. We need something like foldl1WithIndex.
      Tuple i e <- sequential (foldlWithIndex (\i r w ->
        alt (parallel (map (Tuple i) w)) r) empty wscs)
      -- TODO: All the Aff in ws is already discharged. Use a more efficient way than combine to process it
      -- TODO: Also, more importantly, we would like to not have to cancel running fibers unless one of them returns a result
      pure $ combine (fromMaybe wsm (NEA.updateAt i e wsm))


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

mapView :: forall a v1 v2. (v1 -> v2) -> Widget v1 a -> Widget v2 a
mapView f (Widget w) = Widget (hoistFree (mapViewStep f) w)

mapViewStep :: forall v1 v2 a. (v1 -> v2) -> WidgetStep v1 a -> WidgetStep v2 a
mapViewStep f (WidgetStepEff e) = WidgetStepEff e
mapViewStep f (WidgetStepView ws) = WidgetStepView ( ws { view = f ws.view })

display :: forall a v. v -> Widget v a
display v = Widget (liftF (displayStep v))

-- Sync eff
effAction ::
  forall a v.
  Effect a ->
  Widget v a
effAction = Widget <<< liftF <<< WidgetStepEff

-- Async aff
affAction ::
  forall a v.
  v ->
  Aff a ->
  Widget v a
affAction v aff = Widget $ wrap $ WidgetStepEff do
  var <- EVar.empty
  runAff_ (handler var) aff
  -- Detect synchronous resolution
  ma <- EVar.tryTake var
  pure case ma of
    Just a -> pure a
    Nothing -> liftF $ WidgetStepView { view: v, cont: AVar.take var }
  where
  -- TODO: allow client code to handle aff failures
  handler _ (Left e) = log ("Aff failed - " <> show e)
  handler var (Right a) = void (EVar.tryPut a var)

-- Async callback
asyncAction ::
  forall v a.
  v ->
  ((Either Error a -> Effect Unit) -> Effect (Effect Unit)) ->
  Widget v a
asyncAction v handler = affAction v (makeAff (map effectCanceler <<< handler))

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

instance widgetMonadAff :: (Monoid v) => MonadAff (Widget v) where
  liftAff = affAction mempty
    -- Widget $ liftF $ WidgetStep $ Right { view: mempty, cont: aff }
