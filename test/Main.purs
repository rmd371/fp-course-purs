module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ListSpec (listSpec)
import Test.OptionalSpec (optionalSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  optionalSpec
  listSpec
  --log "🍝"
