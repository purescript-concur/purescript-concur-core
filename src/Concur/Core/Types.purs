module Concur.Core.Types where

import Control.Alternative (class Alternative)
import Control.Applicative (class Applicative, pure, (*>))
import Control.Apply (class Apply)
import Control.Bind (class Bind, bind, discard, (>>=))
import Control.Monad (class Monad, ap, join)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Plus (class Alt, class Plus, empty)
import Control.ShiftMap (class ShiftMap)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Function (identity, ($), (<<<))
import Data.Functor (class Functor, void, ($>))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Semigroup (class Semigroup, append)
import Data.TraversableWithIndex (forWithIndex)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref

-- | Callback -> Effect Canceler (returns the unused effect)
-- | Canceling will *always* have some leftover effect, else it would have ended already
newtype Callback a = Callback (Callback' a)
type Callback' a = (a -> Effect Unit) -> Effect Canceler
type Canceler = Effect Unit

data Result v a = View v | Completed a

instance functorResult :: Functor (Result v) where
  map _ (View v) = View v
  map f (Completed a) = Completed (f a)

mkCallback :: forall a. Callback' a -> Callback a
mkCallback = Callback

runCallback :: forall a. Callback a -> Callback' a
runCallback (Callback f) = f

instance functorCallback :: Functor Callback where
  map f c = mkCallback \cb -> runCallback c (cb <<< f)

-- | A callback that will never be resolved
never :: forall a. Callback a
never = mkCallback mempty

-- NOTE: We currently have no monadic instance for callbacks
-- Remember: The monadic instance *must* agree with the applicative instance

instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

-- A Widget is basically a callback that returns a view or a return value
newtype Widget v a = Widget (Callback (Result v a))

derive instance functorWidget :: Functor (Widget v)

instance newtypeWidget :: Newtype (Widget v a) (Callback (Result v a))

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
  pure a = mkWidget \cb -> cb (Completed a) $> pure mempty

instance monadRecWidget :: MonadRec (Widget v) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance bindWidget :: Bind (Widget v) where
  bind m f = mkWidget \cb -> do
    -- CancelerRef starts out as a canceler for A, then becomes canceler for B
    cancelerRef <- Ref.new mempty
    let runCancelers = join (Ref.read cancelerRef)
    cancelerA <- runWidget m \res -> do
      case res of
        View v -> cb (View v)
        Completed a -> do
          runCancelers
          cancelerB <- runWidget (f a) cb
          Ref.write cancelerB cancelerRef
    Ref.write cancelerA cancelerRef

    pure runCancelers

display :: forall a v. v -> Widget v a
display v = mkWidget \cb -> do
  cb (View v)
  pure mempty

-- Ignore all callbacks on a widget
silence :: forall a v. Widget v a -> Widget v a
silence w = mkWidget \_cb -> do
  runWidget w \_res -> void $ pure never

-- Util
flipEither :: forall a b. Either a b -> Either b a
flipEither (Left a) = Right a
flipEither (Right b) = Left b

instance widgetMultiAlternative :: Monoid v => MultiAlternative (Widget v) where
  orr :: forall a. Array (Widget v a) -> Widget v a
  orr widgets = mkWidget \cb -> do
    viewsRef <- Ref.new (A.replicate (A.length widgets) mempty)
    cancelerRef <- Ref.new mempty
    let runCancelers = join (Ref.read cancelerRef)
    _ <- forWithIndex widgets \i w -> do
      c <- runWidget w \res -> do
        views <- Ref.read viewsRef
        case res of
          View v -> do
            let mviews = A.updateAt i v views
            case mviews of
              Nothing -> pure unit
              Just views' -> do
                Ref.write views' viewsRef
                cb (View (fold views'))
          Completed a -> do
            runCancelers
            cb (Completed a)
      Ref.modify (_ *> c) cancelerRef
    pure runCancelers

instance widgetSemigroup :: (Monoid v) => Semigroup (Widget v a) where
  append w1 w2 = orr [ w1, w2 ]

instance widgetMonoid :: (Monoid v) => Monoid (Widget v a) where
  mempty = empty

instance widgetAlt :: (Monoid v) => Alt (Widget v) where
  alt = append

instance widgetPlus :: (Monoid v) => Plus (Widget v) where
  empty = display mempty

instance widgetAlternative :: (Monoid v) => Alternative (Widget v)

-- Sync eff
instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect eff = mkWidget \cb -> do
    a <- eff
    cb (Completed a)
    pure mempty
