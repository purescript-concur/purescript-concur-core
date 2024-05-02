module Concur.Core.Patterns where

import Concur.Core (Result(..), Widget, mkWidget)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Data.Array as Array
import Data.Function (($))
import Data.Monoid (class Monoid)
import Data.Semigroup ((<>))
import Data.Traversable (traverse_)
import Data.Unit (Unit)
import Data.Void (Void)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Unsafe.Reference (unsafeRefEq)

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

