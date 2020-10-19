module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.OptionalSpec (optionalSpec)

main :: Effect Unit
main = do
  optionalSpec
  log "🍝"
