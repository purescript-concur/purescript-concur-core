module Concur.Core.Types where

import Prelude

import Control.ShiftMap (class ShiftMap)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (sequence, for)
import Data.FoldableWithIndex (traverseWithIndex_)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, runAff_, runAff, killFiber)
import Effect.Console (log)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
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

mkCallback :: forall a. Callback' a -> Callback a
mkCallback = Callback

runCallback :: forall a. Callback a -> Callback' a
runCallback (Callback f) = f

instance functorCallback :: Functor Callback where
  map f g = mkCallback \cb -> map (map f) <$> runCallback g (cb <<< f)

display :: forall a v. v -> Widget v a
display v = mkWidget \cb -> do
  cb (Left v)
  pure (pure (unWid (display v)))

-- | A callback that will never be resolved
never :: forall a. Callback a
never = mkCallback \_cb -> pure (pure never)

-- NOTE: We currently have no monadic instance for callbacks
-- Remember: The monadic instance *must* agree with the applicative instance

instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

-- A Widget is basically a callback that returns a view or a return value
newtype Widget v a = Widget (Callback (Either v a))
derive instance functorWidget :: Functor (Widget v)
instance newtypeWidget :: Newtype (Widget v a) (Callback (Either v a)) where
   unwrap = unWid
   wrap = mkWidget <<< runCallback

unWid :: forall v a. Widget v a -> Callback (Either v a)
unWid (Widget w) = w

runWidget :: forall v a. Widget v a -> Callback' (Either v a)
runWidget (Widget (Callback e)) = e

mkWidget :: forall v a. Callback' (Either v a) -> Widget v a
mkWidget e = Widget (Callback e)

instance applyWidget :: Apply (Widget v) where
  apply = ap

instance widgetMonad :: Monad (Widget v)

instance applicativeWidget :: Applicative (Widget v) where
  pure a = mkWidget \cb -> cb (Right a) $> pure never

instance bindWidget :: Bind (Widget v) where
  bind m f = mkWidget \cb -> do
    let cancelerRef = Ref.new Nothing
    r <- cancelerRef
    -- CancelerRef starts out as a canceler for A, then becomes canceler for B
    cancelerA <- runWidget m \res -> do
      case res of
        Left v -> cb (Left v)
        Right a -> do
          -- After A has been resolved, the canceler just becomes a canceler for B
          -- TODO: Should cancelerA also be cancelled here?
          --   Depends on what the ideal API contract is. INVESTIGATE.
          cancelerB <- runWidget (f a) cb
          Ref.write (Just cancelerB) r

      -- The initial canceler just cancels A, and then binds the remaining widget with B
    val <- Ref.read r
    pure $ case val of
      Just canceler -> canceler
      Nothing -> do
        c <- cancelerA
        pure (unWid (bind (Widget c) f))

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
    cRef <- init widgets (pure never)
    wRef <- init widgets (Left mempty)
    traverseWithIndex_ (subscribe cb wRef cRef) widgets
    cancelers <- Ref.read cRef
    wi <- sequence cancelers
    pure $ pure (unWid (orr $ Widget <$> wi))
    where
      init :: forall a b. Array (Widget v a) -> b -> Effect (Ref (Array b))
      init ws x = Ref.new $ A.replicate (A.length ws) x
      subscribe callback widgetsRef cancelersRef i w = do
        canceler <- runWidget w \res -> do
          es <- Ref.modify (\s -> fromMaybe s $ A.updateAt i res s) widgetsRef
          go callback mempty es cancelersRef
        void $ Ref.modify (\s -> fromMaybe s $ A.updateAt i canceler s) cancelersRef
      go callback v es cs = case A.uncons es of
        Just { head, tail } -> case head of
          Left va -> go callback (v <> va) tail cs
          Right a -> do
            callback (Right a)
            -- Runs all cancelers after the callback returns a value
            -- I feel like I am not using this the way it was intended
            -- But it works, in the case of competing Affs, for example.
            cancelers <- Ref.read cs
            void $ for cancelers \n -> do
              inner <- n
              void $ runCallback inner \_ -> pure unit
        Nothing -> callback (Left v)

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
  cb (Right inner)
  pure (pure (unWid $ effAction a))

-- Sync eff

killAff ::
  forall a v.
  v ->
  Fiber Unit ->
  Callback (Either v a)
killAff v f = mkCallback \cb -> do
  cb (Left v)
  let aff = killFiber (error "cancelling aff") f
  runAff_ (handler cb) aff
  pure (pure never)
  where
    handler cb (Right r) = pure unit
    handler cb (Left _) = pure unit

affAction ::
  forall a v.
  v ->
  Aff a ->
  Widget v a
affAction v aff = mkWidget \cb -> do
  cb (Left v)
  fiber <- runAff (handler cb) aff
  pure (pure (killAff v fiber))
  where
    handler cb (Right r) = cb (Right r)
    handler cb (Left _) = log "error calling aff"

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

instance widgetMonadAff :: (Monoid v) => MonadAff (Widget v) where
  liftAff = affAction mempty

data DebounceStatus = Initial | Waiting TimeoutId | Elapsed

debounced ::
  forall a v.
  Int ->
  Widget v a ->
  Widget v a
debounced timeout w =
  let idRef = Ref.new Initial in do
  mkWidget \cb -> do
    idRefInner <- idRef
    runWidget w (hdlr cb timeout idRefInner)
  where
    hdlr cb time ref = \res -> case res of
      Left l -> cb (Left l)
      Right r -> debounceInner timeout cb ref r

debounceInner ::
  forall a v.
  Int ->
  (Either v a -> Effect Unit) ->
  Ref DebounceStatus ->
  a ->
  Effect Unit
debounceInner time callback ref a = do
  id <- Ref.read ref
  case id of
    Initial -> schedule callback time ref a
    Waiting tid -> do
      clearTimeout tid
      schedule callback time ref a
    Elapsed -> callback (Right a)
  where
    schedule cb t r v = do
      tid <- setTimeout time do
        Ref.write Elapsed ref
        debounceInner t cb r v
      Ref.write (Waiting tid) ref
