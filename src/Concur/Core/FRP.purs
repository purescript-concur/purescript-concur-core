module Concur.Core.FRP where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Cofree (Cofree, mkCofree, tail)
import Control.Comonad (extract)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

----------
-- SIGNALS
----------
-- | Poor man's FRP implementation for Concur.
-- | I am experimenting with the smallest possible amount of FRP which can still be useful.
-- | A Widget can be considered to be a one-shot Event. (There is no stream of events in Concur).
-- | Signals then are never-ending widget loops that allow access to their last return value.
-- | This last produced value allows composition with other widgets even for never-ending widgets.
type Signal m a = Cofree m a

-- | Construct a signal from an initial value, and a step widget
step ::
  forall m a.
  a ->
  m (Signal m a) ->
  Signal m a
step = mkCofree



-- | Display a widget which returns a continuation
display :: forall m. m (Signal m Unit) -> Signal m Unit
display w = step unit w

-- | Fires a widget once then stop. This will reflow when a parent signal reflows
-- | Starts as Nothing. Then switches to `Just returnVal` after the Widget is done
fireOnce :: forall m a. Monad m => Plus m => m a -> Signal m (Maybe a)
fireOnce w = step Nothing do
  a <- w
  pure (step (Just a) empty)

-- | Similar to `fireOnce`, but discards the return value
fireOnce_ :: forall m. Monad m => Plus m => m Unit -> Signal m Unit
fireOnce_ w = display do w *> empty

-- | Wait until we get a `Just` value from a signal
justWait :: forall m a b.
            Monad m =>
            Alternative m =>
            b -> Signal m (Maybe a) -> (a -> Signal m b) -> Signal m b
justWait b s f = do
  m <- s
  case m of
    Nothing -> pure b
    Just a -> f a

-- | Run an effectful computation, and do something with the result
justEffect :: forall m a b. MonadEffect m => Monad m => Alternative m => b -> Effect a -> (a -> Signal m b) -> Signal m b
justEffect b e f = justWait b (fireOnce do liftEffect e) f

-- | A constant signal
always ::
  forall m a.
  Monad m =>
  Alternative m =>
  a ->
  Signal m a
always = pure

-- | Update signal to a new value
update ::
  forall m a.
  Signal m a ->
  m (Signal m a)
update = tail

-- | Construct a signal by polling a signal with a nested widget for values
poll ::
  forall m a.
  Monad m =>
  Signal m (m a) ->
  m (Signal m a)
poll b = step <$> extract b <*> (map poll (update b))

-- | Create a signal which repeatedly invokes a widget for values.
-- | E.g. `signal False checkbox` will return a signal which reflects the current value of the checkbox.
hold ::
  forall m a.
  Monad m =>
  a ->
  m a ->
  Signal m a
hold a w = step a do
  a' <- w
  pure (hold a' w)

-- | Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
loopW ::
  forall m a.
  Monad m =>
  a ->
  (a -> m a) ->
  Signal m a
loopW a f = step a (go <$> f a)
  where
  go x = loopW x f

-- | Loop a signal so that the return value is passed to the beginning again.
loopS ::
  forall m a.
  Monad m =>
  a ->
  (a -> Signal m a) ->
  Signal m a
loopS a f = step (extract this) do
  s <- update this
  pure (loopS (extract s) f)
  where
  this = f a

-- | Loop a signal so that the return value is passed to the beginning again.
-- loop :: forall m a. Monoid v => (a -> Signal m (Maybe a)) -> Signal m a
-- loop f = step (extract this) do
--   s <- update this
--   pure (loopS (extract s) f)
--   where this = f Nothing
-- | Folding signals. Similar to how signals used to work in Elm.
-- | This can be used to implement simple stateful Signals.
-- | e.g. `counter = fold (\n _ -> n+1) 0 clicks`
foldp ::
  forall m a b.
  Functor m =>
  (a -> b -> a) ->
  a ->
  Signal m b ->
  Signal m a
foldp f a sb = step a' (map (foldp f a') (update sb))
  where
  a' = f a (extract sb)

-- | Consume a closed signal to make a widget
-- dyn :: forall v. (forall x. Signal m x) ~> (forall x. m x)
dyn ::
  forall m a b.
  Monad m =>
  Signal m a ->
  m b
dyn s = update s >>= dyn

-- | Run a signal *once* and return its value
oneShot ::
  forall m a.
  Monad m =>
  Signal m (Maybe a) ->
  m a
oneShot s = case extract s of
  Nothing -> update s >>= oneShot
  Just a -> pure a

-- Very useful to embed a signal in the middle of a widget
demand ::
  forall m a.
  Monad m =>
  Signal m (Maybe a) ->
  m a
demand = oneShot

demand' :: forall m a. Monad m => (Maybe a -> Signal m (Maybe a)) -> m a
demand' f = oneShot (f Nothing)

-- A Common pattern is demand + stateLoopS
demandLoop ::
  forall m a s.
  Monad m =>
  Alternative m =>
  s ->
  (s -> Signal m (Either s a)) ->
  m a
demandLoop def w = demand (stateLoopS def w)

-- A generalisation of `loopS` where, you have an inner loop state `s` and a final result `a`
-- The loop continues as long as `Left s` is returned. And ends when `Right a` is returned.
stateLoopS ::
  forall m a s.
  Monad m =>
  Alternative m =>
  s ->
  (s -> Signal m (Either s a)) ->
  Signal m (Maybe a)
stateLoopS def w = map hush $ loopS (Left def) $ either w (pure <<< Right)


-- Debounced output from a widget
-- wrapped into a signal
debounce :: forall m a. Monad m => Alt m => MonadAff m =>
            Number -> a -> (a -> m a) -> Signal m a
debounce timeoutMs ainit winit = go ainit winit
  where
    go a w = step a do
      -- Wait until we have a user input
      -- before starting the timer
      a' <- w a
      go' a' w
    go' a w = do
      res <- (Just <$> w a) <|> (Nothing <$ liftAff (delay (Milliseconds timeoutMs)))
      case res of
        -- Timeout fired
        Nothing -> pure (go a w)
        -- Events fired, but we are still in timeout
        Just a' -> go' a' w
