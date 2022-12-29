module Concur.Core.Props where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (write, read, new) as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import Effect.Unsafe (unsafePerformEffect)

data Props p a
  = PrimProp p
  | Handler ((a -> Effect Unit) -> p)

instance functorProps :: Functor (Props p) where
  map _ (PrimProp p) = PrimProp p
  map f (Handler h) = Handler \k -> h (k <<< f)

-- | Internal. Do not use. Use unsafeMkProp, or unsafeMkPropHandler instead.
mkProp
  :: forall p a
  . (a -> Effect Unit)
  -> Props p a
  -> p
mkProp _ (PrimProp a) = a
mkProp h (Handler f) = f h

-- | Use `handleProp` to handle an event manually
handleProp
  :: forall p a b
  .  (a -> Effect Unit)
  -> Props p a
  -> Props p b
handleProp _ (PrimProp p) = PrimProp p
handleProp f (Handler g) = PrimProp (g f)

-- | Use this to filter the output of an event handler prop.
-- | For example, to only handle the enter key - `filterProp isEnterEvent onKeyDown`
filterProp ::
  forall p a.
  (a -> Boolean) ->
  Props p a ->
  Props p a
filterProp _ p@(PrimProp _) = p
filterProp ok (Handler g) = Handler \h ->
  (g \a -> if ok a
      then h a
      else pure unit)

debounce :: forall p a.
   Int ->
   Props p a ->
   Props p a
debounce t p =
  let ref = unsafePerformEffect $ Ref.new Nothing in
  case p of
    PrimProp pp -> PrimProp pp
    Handler hdlr -> Handler \cb -> hdlr (debounceInner t ref cb)

debounceInner ::
  forall a.
  Int ->
  Ref (Maybe TimeoutId) ->
  (a -> Effect Unit) ->
  a ->
  Effect Unit
debounceInner time ref callback a = do
  id <- Ref.read ref
  case id of
    Nothing -> schedule callback time ref a
    Just tid -> do
      clearTimeout tid
      schedule callback time ref a
  where
    schedule cb t r v = do
      tid <- setTimeout time $ callback v
      Ref.write (Just tid) ref
