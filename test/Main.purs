module Test.Main where

import Prelude

import Course.ListSpec (listSpec)
import Effect (Effect)
import Effect.Class.Console (log)
--import Test.OptionalSpec (optionalSpec)

main :: Effect Unit
main = do
  --optionalSpec
  listSpec
  log "🍝"
