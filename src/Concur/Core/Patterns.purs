module Concur.Core.Patterns where

import Concur.Core (Result(..), Widget, mkWidget)
import Control.Alt (class Alt, (<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad (class Monad)
import Control.Plus (class Plus)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Function (flip, ($), (<<<), (>>>))
import Data.Functor ((<$>))
import Data.Lens (Lens')
import Data.Lens as L
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, un)
import Data.Semigroup ((<>))
import Data.Traversable (traverse_)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Unsafe.Reference (unsafeRefEq)

-- | A very useful combinator for widgets with localised state
loopState :: forall m a s. Monad m => s -> (s -> m (Either s a)) -> m a
loopState s f = f s >>= case _ of
  Left s' -> loopState s' f
  Right a -> pure a

-- | Repeat a computation until the value satisfies a predicate
retryUntil :: forall m a. Monad m => (a -> Boolean) -> m a -> m a
retryUntil p w = w >>= \a -> if p a then pure a else retryUntil p w

-- | Repeat a computation until the value satisfies a predicate, looping in the previous value
retryUntilLoop :: forall m a. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
retryUntilLoop p w a = w a >>= \a' -> if p a' then pure a' else retryUntilLoop p w a'

-- | The Elm Architecture
tea :: forall a s m x. Monad m
  => s
  -> (s -> m a)
  -> (a -> s -> s)
  -> m x
tea s render update = go s
  where
  go st = render st >>= (flip update st >>> go)

type RemoteInterface v a =
  { yield :: Widget v a
  , render :: Widget v Unit
  }

-- | Separate the UI from the widget result
remoteWidget
  :: forall m v a
  .  Monoid v
  => MonadEffect m
  => Widget v a
  -> m (RemoteInterface v a)
remoteWidget axn = do
  remoteCb <- liftEffect $ Ref.new []
  pure { yield: yield remoteCb, render: render remoteCb }
  where
  yield remoteCb = mkWidget \cb -> do
    Ref.modify_ (_ <> [cb]) remoteCb
    pure do
      Ref.modify_ (Array.deleteBy unsafeRefEq cb) remoteCb
  render remoteCb = do
    val <- axn
    liftEffect do
       Ref.read remoteCb >>= traverse_ \cb -> cb (Completed val)

-- | Internalise state
-- The resulting widget can be rescheduled multiple
-- times and will retain its state internally
internalise
  :: forall m v a b
  .  Monoid v
  => MonadEffect m
  => (a -> Widget v a)
  -> a
  -> m (Widget v b)
internalise axn val = do
  ref <- liftEffect $ Ref.new val
  pure (render ref)
  where
  render ref = liftEffect (Ref.read ref) >>= go
    where
    go v = do
      v' <- axn v
      liftEffect do Ref.write v' ref
      go v'

-- WORKING WITH LOCAL ENVIRONMENTS

-- | A wire can send values up into a local environment
newtype Wire m a = Wire
  { value :: Effect a
  , send :: a -> Effect Unit
  , receive :: m a
  }

derive instance newtypeWire :: Newtype (Wire m a) _

-- | Map a Lens over a Wire
mapWire :: forall m s a. Alt m => MonadEffect m => Plus m => Lens' s a -> Wire m s -> Wire m a
mapWire lens wire =
  let wirerec = un Wire wire
  in Wire { value: L.view lens <$> wirerec.value
          , send: \a -> wirerec.value >>= (wirerec.send <<< L.set lens a)
          , receive: L.view lens <$> wirerec.receive
          }

-- | Setup a local environment with a wire
local :: forall v r a. Monoid v => a -> (Wire (Widget v) a -> Widget v r) -> Widget v r
local ainit f = do
  currentVal <- liftEffect $ Ref.new ainit
  remoteCb <- liftEffect $ Ref.new $ pure $ case _ of
     Completed a -> Ref.write a currentVal
     _ -> pure unit
  let wire = Wire
       { value: Ref.read currentVal
       , send: \a -> do
           Ref.read remoteCb >>= traverse_ \cb -> cb (Completed a)
       , receive: mkWidget \cb -> do
           Ref.modify_ (_ <> [cb]) remoteCb
           pure do 
              Ref.modify_ (Array.deleteBy unsafeRefEq cb) remoteCb
       }
  f wire

send :: forall m a. MonadEffect m => Plus m => Wire m a -> a -> m Unit
send w = liftEffect <<< (un Wire w).send

receive :: forall v a. Wire (Widget v) a -> Widget v a
receive w = (un Wire w).receive

with :: forall v r a. Monoid v => Wire (Widget v) a -> (a -> Widget v r) -> Widget v r
with wire f = do
  val <- liftEffect $ (un Wire wire).value
  go val
  where
  go a = do
    res <- (Left <$> f a) <|> (Right <$> receive wire)
    either pure go res

