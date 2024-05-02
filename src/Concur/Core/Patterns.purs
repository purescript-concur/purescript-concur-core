module Concur.Core.Patterns where

import Concur.Core (Result(..), Widget, mkWidget)
import Control.Alternative (empty)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Data.Array as Array
import Data.Function (($))
import Data.Monoid (class Monoid)
import Data.Semigroup ((<>))
import Data.Traversable (traverse_)
import Data.Void (Void)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Unsafe.Reference (unsafeRefEq)

type RemoteInterface v a =
  { yield :: Widget v a
  , render :: Widget v Void
  }

-- | Separate the UI from the widget result
remoteWidget
  :: forall v a
  .  Monoid v
  => Widget v a
  -> Widget v (RemoteInterface v a)
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
    empty

