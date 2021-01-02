module Test.Main where

import Prelude

import Course.MonadSpec (spec) as M
import Course.ApplicativeSpec (spec) as A
import Course.StateSpec (spec) as S
import Course.FunctorSpec (functorSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ListSpec (listSpec)
import Test.OptionalSpec (optionalSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  --optionalSpec
  --listSpec
  --functorSpec
  -- A.spec
  -- M.spec
  S.spec
  --log "🍝"
