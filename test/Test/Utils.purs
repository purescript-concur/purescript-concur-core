module Test.Utils where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Types (WidgetStep(..), unWidget)
import Control.Monad.Free (runFreeM)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (singleton)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)


-- Evalutates Widget to Aff
-- Be carefull that never ending Widget will convert to never ending Aff.
runWidgetAsAff :: forall v a. Widget v a -> Aff { result :: a, views :: Array v }
runWidgetAsAff widget = do
  Tuple result views <- runWriterT $ runFreeM interpret (unWidget widget)
  pure { result, views }
  where
    interpret (WidgetStepEff eff) =
      liftEffect eff

    interpret (WidgetStepView rec) = do
      tell $ singleton rec.view
      liftAff rec.cont
