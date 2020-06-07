module Test.WidgetSpec where

import Prelude

import Concur.Core.Types (affAction)
import Control.MultiAlternative (orr)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Utils (runWidgetAsAff)

widgetSpec :: Spec Unit
widgetSpec =
  describe "Widget" do
    describe "orr" do
      it "should cancel running effects when the widget returns a value" do
        ref <- liftEffect $ Ref.new ""
        { views } <- runWidgetAsAff $ orr
          [ affAction "a" do
               delay (Milliseconds 100.0)
               liftEffect $ Ref.write "a" ref
          , affAction "b" do
               delay (Milliseconds 150.0)
               liftEffect $ Ref.write "b" ref
          ]
        views `shouldEqual` [ "ab" ]
        liftEffect (Ref.read ref) `shouldReturn` "a"
        delay (Milliseconds 100.0)
        liftEffect (Ref.read ref) `shouldReturn` "a"
