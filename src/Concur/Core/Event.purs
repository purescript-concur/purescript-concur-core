module Concur.Core.Event where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<))
import Control.Monad (when)
import Data.Eq ((/=))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Function (($))
import Data.Functor (class Functor)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event

-- Chain an effect onto an observer
effMap :: forall a b. Event a -> (a -> Effect b) -> Event b
effMap event f = makeEvent \cb -> subscribe event \a -> cb =<< f a

-- TODO: Monadic chaining for observer
-- instance observeApply :: Apply Event where
--   -- apply :: forall a b. f (a -> b) -> f a -> f b
--   apply (Event f) (Event a) = Event \cb -> f \cf' ->

-- subscribe
-- | observe :: forall a. Event a -> (a -> Effect Unit) -> Effect (Effect Unit)
-- | observe (Event f) = f

-- | empty
-- | never :: forall a. Event a
-- | never = Event \_ -> pure (pure unit)

-- | dont :: forall a. Pusher a
-- | dont a = pure unit

-- Push data
-- | type Pusher a = a -> Effect Unit


-----------------------------------------------------------------------
----------------------- LOOK FOR BUGS HERE ----------------------------
--- WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING ---
-- BELOW ARE THE ONLY MUTABLE, STATEFUL BITS IN THE ENTIRE FRAMEWORK --
-----------------------------------------------------------------------

parIndex :: forall a. Array (Event a) -> Event ({i::Int, val::a})
parIndex = par' \i val -> {i, val}

par :: forall a. Array (Event a) -> Event a
par = par' \_ val -> val

par' :: forall a b. (Int -> a -> b) -> Array (Event a) -> Event b
par' g os = makeEvent \cb -> do
  ref <- Ref.new []
  cs <- traverseWithIndex (\i event -> subscribe event \a -> do
          cs <- Ref.read ref
          Ref.write [] ref
          traverseWithIndex_ (\j -> when (j /= i)) cs
          cb $ g i a
    ) os
  Ref.write cs ref
  pure do
    sequence_ cs
    Ref.write [] ref

-- | mkObserver :: forall a. Effect { push :: Pusher a, subscribe :: Event a }
-- | mkObserver = do
-- |   ref <- Ref.new Nothing
-- |   let push a = maybe (pure unit) (_ $ a) =<< Ref.read ref
-- |   let subscribe = Event \cb -> do
-- |         Ref.write (Just cb) ref
-- |         pure do Ref.write Nothing ref
-- |   pure { push, subscribe}
