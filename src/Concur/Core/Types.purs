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
import Data.Functor (class Functor, void, ($>), map)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Semigroup (class Semigroup, append)
import Data.TraversableWithIndex (forWithIndex)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref

newtype Callback a = Callback (Callback' a)

derive instance Newtype (Callback a) _

type Callback' a = (a -> Effect Unit) -> Effect Canceler
type Canceler = Effect Unit

data Result v a = View v | Completed a

result :: forall v a r. (v -> r) -> (a -> r) -> Result v a -> r
result f g = case _ of
  View v -> f v
  Completed a -> g a

instance Functor (Result v) where
  map _ (View v) = View v
  map f (Completed a) = Completed (f a)

mapViewResult :: forall u v a. (u -> v) -> Result u a -> Result v a
mapViewResult f = case _ of
  View u -> View (f u)
  Completed a -> Completed a

mkCallback :: forall a. Callback' a -> Callback a
mkCallback = Callback

runCallback :: forall a. Callback a -> Callback' a
runCallback (Callback f) = f

instance Functor Callback where
  map f c = mkCallback \cb -> runCallback c (cb <<< f)

-- | A callback that will never be resolved
never :: forall a. Callback a
never = mkCallback mempty

-- NOTE: We currently have no monadic instance for callbacks
-- Remember: The monadic instance *must* agree with the applicative instance

instance ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

-- A Widget is basically a callback that returns a view or a return value
newtype Widget v a = Widget (Callback (Result v a))

derive instance Functor (Widget v)

derive instance Newtype (Widget v a) _

unWid :: forall v a. Widget v a -> Callback (Result v a)
unWid (Widget w) = w

runWidget :: forall v a. Widget v a -> Callback' (Result v a)
runWidget (Widget (Callback e)) = e

mkWidget :: forall v a. Callback' (Result v a) -> Widget v a
mkWidget e = Widget (Callback e)

mapView :: forall u v a. (u -> v) -> Widget u a -> Widget v a
mapView f (Widget w) = Widget (map (mapViewResult f) w)

instance Apply (Widget v) where
  apply = ap

instance Monad (Widget v)

instance Applicative (Widget v) where
  pure a = mkWidget \cb -> cb (Completed a) $> pure mempty

instance MonadRec (Widget v) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance Bind (Widget v) where
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

data InitialisingRenderState
  = Initialising
  | Aborted
  | Initialised

initialised :: InitialisingRenderState -> InitialisingRenderState
initialised = case _ of
  Initialising -> Initialised
  a -> a

instance Monoid v => MultiAlternative (Widget v) where
  orr :: forall a. Array (Widget v a) -> Widget v a
  orr widgets = mkWidget \cb -> do
    viewsRef <- Ref.new (A.replicate (A.length widgets) mempty)
    cancelerRef <- Ref.new mempty
    renderStateRef <- Ref.new Initialising
    let runCancelers = join (Ref.read cancelerRef)
    _ <- forWithIndex widgets \i w -> do
      c <- runWidget w \res -> case res of
        View v -> do
          views <- Ref.read viewsRef
          case A.updateAt i v views of
            Nothing -> pure unit
            Just views' -> do
              Ref.write views' viewsRef
              renderState <- Ref.read renderStateRef
              case renderState of
                Initialised -> cb (View (fold views'))
                _ -> pure unit
        Completed a -> do
          Ref.write Aborted renderStateRef
          runCancelers
          cb (Completed a)
      Ref.modify (_ *> c) cancelerRef
    Ref.modify_ initialised renderStateRef
    views <- Ref.read viewsRef
    cb (View (fold views))
    pure runCancelers

instance (Monoid v) => Semigroup (Widget v a) where
  append w1 w2 = orr [ w1, w2 ]

instance (Monoid v) => Monoid (Widget v a) where
  mempty = empty

instance (Monoid v) => Alt (Widget v) where
  alt = append

instance (Monoid v) => Plus (Widget v) where
  empty = display mempty

instance (Monoid v) => Alternative (Widget v)

instance (Monoid v) => MonadEffect (Widget v) where
  liftEffect eff = mkWidget \cb -> do
    a <- eff
    cb (Completed a)
    pure mempty
