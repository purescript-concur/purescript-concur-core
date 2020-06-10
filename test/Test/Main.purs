module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.WidgetSpec (widgetSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Concur.Core" do
    widgetSpec
