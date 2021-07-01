module Concur.Core.Types where

import Prelude

import Control.MonadFix (mfix)
import Control.ShiftMap (class ShiftMap)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, runAff_, runAff, killFiber)
import Effect.Console (log)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Control.Alternative (class Alternative)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Plus (class Alt, class Plus, empty)
import Effect.Class (class MonadEffect)

-- | Callback -> Effect Canceler (returns the unused effect)
-- | Canceling will *always* have some leftover effect, else it would have ended already
-- | TODO: Have a way to check if the callback is finished (i.e. will never be called again)
-- |       One option is to have a cb = (Either partResult a -> Effect Unit)
newtype Callback a = Callback (Callback' a)
type Callback' a = (a -> Effect Unit) -> Effect (Effect (Callback a))

data Result v a = View v | Completed a | Partial a

instance functorResult :: Functor (Result v) where
  map f (View v) = View v
  map f (Completed a) = Completed (f a)
  map f (Partial a) = Partial (f a)

mkCallback :: forall a. Callback' a -> Callback a
mkCallback = Callback

runCallback :: forall a. Callback a -> Callback' a
runCallback (Callback f) = f

instance functorCallback :: Functor Callback where
  map f g = mkCallback \cb -> map (map f) <$> runCallback g (cb <<< f)

display :: forall a v. v -> Widget v a
display v = mkWidget \cb -> do
  cb (View v)
  pure (pure (unWid (display v)))

-- | A callback that will never be resolved
never :: forall a. Callback a
never = mkCallback \_cb -> pure (pure never)

-- Ignore all callbacks on a widget
silence :: forall a v. Widget v a -> Widget v a
silence w = mkWidget \cb -> do
  runWidget w \res -> void $ pure never

-- NOTE: We currently have no monadic instance for callbacks
-- Remember: The monadic instance *must* agree with the applicative instance

instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

-- A Widget is basically a callback that returns a view or a return value
newtype Widget v a = Widget (Callback (Result v a))
derive instance functorWidget :: Functor (Widget v)

instance newtypeWidget :: Newtype (Widget v a) (Callback (Result v a)) where
   unwrap = unWid
   wrap = mkWidget <<< runCallback

unWid :: forall v a. Widget v a -> Callback (Result v a)
unWid (Widget w) = w

runWidget :: forall v a. Widget v a -> Callback' (Result v a)
runWidget (Widget (Callback e)) = e

mkWidget :: forall v a. Callback' (Result v a) -> Widget v a
mkWidget e = Widget (Callback e)

instance applyWidget :: Apply (Widget v) where
  apply = ap

instance widgetMonad :: Monad (Widget v)

instance applicativeWidget :: Applicative (Widget v) where
  pure a = mkWidget \cb -> cb (Completed a) $> pure never

instance bindWidget :: Bind (Widget v) where
  bind m f = mkWidget \cb -> do
    syncCanceler <- Ref.new Nothing
    -- CancelerRef starts out as a canceler for A, then becomes canceler for B
    asyncCanceler <- mfix \asyncCanceler -> do
      cancelerA <- runWidget m \res -> do
        case res of
          View v -> cb (View v)
          Completed a -> do
            cancelerB <- runWidget (f a) cb
            Ref.write (Just cancelerB) syncCanceler
          Partial a -> do
            -- After A has been resolved, the canceler just becomes a canceler for B
            -- TODO: Should cancelerA also be cancelled here?
            --   Depends on what the ideal API contract is. INVESTIGATE.
            -- Cancel A first
            cA <- join (Ref.read (asyncCanceler unit))
            void $ runCallback cA \_ -> pure unit
            --
            cancelerB <- runWidget (f a) cb
            void $ Ref.write cancelerB (asyncCanceler unit)

      -- The initial canceler just cancels A, and then binds the remaining widget with B
      Ref.new do
        c <- cancelerA
        pure (unWid (bind (Widget c) f))

    -- The returned canceler just reads the canceler ref and runs it
    scm <- Ref.read syncCanceler
    case scm of
      Just sc -> pure sc
      Nothing -> Ref.read asyncCanceler

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
  orr :: forall v a. Monoid v => Array (Widget v a) -> Widget v a
  orr widgets = mkWidget \cb -> do
    wcRefs <- sequence $ A.replicate (A.length widgets) $ do
       l <- Ref.new $ View mempty
       r <- Ref.new $ never
       pure $ Tuple l r
    subscribed <- Ref.new false
    let results = map fst wcRefs
    traverse_ (subscribe subscribed cb results) $ A.zip widgets wcRefs
    -- Allow bubbing of callbacks up only *after* they are all hooked.
    -- This is because view callbacks i.e cb (Left v) fire on creation of a widget.
    -- This is controlled using `subscribed` ref.
    Ref.write true subscribed
    let cancelers = map (Ref.read <<< snd) wcRefs
    wi <- sequence cancelers
    step cb mempty results
    pure $ pure (unWid (orr $ Widget <$> wi))

-- This is not pretty but works
-- Idea is to aggregate the view or fire cb if we get a result.
-- It would be nice to be able to bail out early without traversing the
-- whole array when we get a result
foldStep ::
  forall v a.
  Monoid v =>
  (Result v a -> Effect Unit) ->
  Effect (Maybe v) ->
  Ref (Result v a) ->
  Effect (Maybe v)
foldStep callback lastE next = do
  last <- lastE
  case last of
    Just v -> do
      n <- Ref.read next
      case n of
        View va -> pure $ Just $ v <> va
        Partial a -> do
          callback (Partial a)
          pure Nothing
        Completed a -> do
          callback (Completed a)
          pure Nothing
    Nothing -> pure Nothing

step ::
  forall a v.
  Monoid v =>
  Semigroup v =>
  (Result v a -> Effect Unit) ->
  v ->
  Array (Ref (Result v a)) ->
  Effect Unit
step callback v resultRefs  = do
  maybeView  <- foldl (foldStep callback) (pure $ Just mempty) resultRefs
  case maybeView of
    Just view -> callback (View view)
    Nothing -> pure unit

subscribe ::
  forall a v.
  Semigroup v =>
  Monoid v =>
  Ref Boolean ->
  (Result v a -> Effect Unit) ->
  Array (Ref (Result v a)) ->
  Tuple (Widget v a) (Tuple (Ref (Result v a)) (Ref (Callback (Result v a)))) ->
  Effect Unit
subscribe ss callback results wrTpl = do
  let refs = snd wrTpl
  canceler <- runWidget (fst wrTpl) \res -> do
    Ref.write res (fst refs)
    subs <- Ref.read ss
    case subs of
      true  -> step callback mempty results
      false -> pure unit
  inner <- canceler
  Ref.write inner (snd refs)

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

effAction ::
  forall a v.
  Effect a ->
  Widget v a
effAction a = mkWidget \cb -> do
  inner <- a
  cb (Completed inner)
  pure (pure (never))

-- Sync eff

killAff ::
  forall a v.
  v ->
  Fiber Unit ->
  Callback (Result v a)
killAff v f = mkCallback \cb -> do
  cb (View v)
  let aff = killFiber (error "cancelling aff") f
  runAff_ (handler cb) aff
  pure (pure never)
  where
    handler cb _ = pure unit

affAction ::
  forall a v.
  v ->
  Aff a ->
  Widget v a
affAction v aff = mkWidget \cb -> do
  cb (View v)
  fiber <- runAff (handler cb) aff
  pure (pure (killAff v fiber))
  where
    handler cb (Right r) = cb (Completed r)
    handler cb (Left _) = log "error calling aff"

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

instance widgetMonadAff :: (Monoid v) => MonadAff (Widget v) where
  liftAff = affAction mempty
