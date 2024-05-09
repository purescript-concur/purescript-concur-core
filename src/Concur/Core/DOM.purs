module Concur.Core.DOM where

import Concur.Core (mkLeafWidget, mkNodeWidget)
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Types (Widget)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Function (($), (<<<))
import Data.Functor (class Functor, map)
import Data.Unit (Unit)
import Effect (Effect)

viewAdapter
  :: forall ps vs res
   . (ps -> vs -> res)
  -> (ps -> vs -> Array res)
viewAdapter f = \ps vs -> [ f ps vs ]

-- | Wrap a single widget with a node that can have eventHandlers attached
el
  :: forall f p v m a
   . ShiftMap (Widget v) m
  => Functor f
  => Functor p
  => (f (p (Effect Unit)) -> v -> v)
  -> f (p a)
  -> m a
  -> m a
el e props = shiftMap (\f w -> mkNodeWidget (\h v -> (e (map (map (h <<< f)) props) v)) w)

-- | el for array views
elArr
  :: forall f p v m a
   . ShiftMap (Widget (Array v)) m
  => Functor f
  => Functor p
  => (f (p (Effect Unit)) -> Array v -> v)
  -> f (p a)
  -> m a
  -> m a
elArr e props = shiftMap (\f w -> mkNodeWidget (\h v -> (viewAdapter e (map (map (h <<< f)) props) v)) w)

-- | Promote a leaf node to a widget
elLeaf
  :: forall f p v m a
   . LiftWidget v m
  => Functor f
  => Functor p
  => (f (p (Effect Unit)) -> v)
  -> f (p a)
  -> m a
elLeaf e props = liftWidget $ mkLeafWidget \h -> e (map (map h) props)

-- | elLeaf for array views
elLeafArr
  :: forall f p v m a
   . LiftWidget (Array v) m
  => Functor f
  => Functor p
  => (f (p (Effect Unit)) -> v)
  -> f (p a)
  -> m a
elLeafArr e props = liftWidget $ mkLeafWidget \h -> [ e (map (map h) props) ]

-- | Wrap some widgets with a node that can have eventHandlers attached
el'
  :: forall f p v m a
   . ShiftMap (Widget v) m
  => MultiAlternative m
  => Functor f
  => Functor p
  => (f (p (Effect Unit)) -> v -> v)
  -> f (p a)
  -> Array (m a)
  -> m a
el' e props = el e props <<< orr

-- | el' for array views
elArr'
  :: forall f p v m a
   . ShiftMap (Widget (Array v)) m
  => MultiAlternative m
  => Functor f
  => Functor p
  => (f (p (Effect Unit)) -> Array v -> v)
  -> f (p a)
  -> Array (m a)
  -> m a
elArr' e props = el (viewAdapter e) props <<< orr

