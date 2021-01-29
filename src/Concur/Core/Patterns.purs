module Concur.Core.Patterns where

import Prelude
import Data.Either (Either(..))

-- | A very useful combinator for widgets with localised state
loopState ::
  forall m a s.
  Monad m =>
  s ->
  (s -> m (Either s a)) ->
  m a
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
tea ::
  forall a s m x.
  Monad m =>
  s ->
  (s -> m a) ->
  (a -> s -> s) ->
  m x
tea s render update = go s
  where
  go st = render st >>= (flip update st >>> go)

-- WORKING WITH LOCAL ENVIRONMENTS

{-
-- | A wire can send values up into a local environment
type Wire m a = { value :: a, send :: a -> m Void, receive :: m a }

-- | Map a Lens over a Wire
mapWire :: forall m s a. Functor m => Lens' s a -> Wire m s -> Wire m a
mapWire lens wire =
  { value: L.view lens wire.value
  , send: \a -> wire.send $ L.set lens a wire.value
  , receive: map (L.view lens) wire.receive
  }

-- | Setup a local environment with a wire
local :: forall m r a. Alt m => MonadEffect m => MonadAff m => Plus m => a -> (Wire m a -> m r) -> m r
local a f = do
  var <- liftEffect EVar.empty
  go { value: a
     , send: \a' -> liftAff (AVar.put a' var) *> empty
     , receive: liftAff $ AVar.take var
     }
  where
  updateWire wire a' = wire {value=a'}
  go wire = do
    res <- (Left <$> f wire) <|> (Right <$> wire.receive)
    either pure (go <<< updateWire wire) res
-}
