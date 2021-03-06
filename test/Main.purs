module Test.Main where

import Prelude

import Course.MonadSpec (spec) as M
import Course.ExtendSpec (spec) as E
import Course.ComonadSpec (spec) as C
import Course.TraversableSpec (spec) as T
import Course.ApplicativeSpec (spec) as A
import Course.StateSpec (spec) as S
import Course.StateTSpec (spec) as ST
import Course.ListZipperSpec (spec) as LZ
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
  --S.spec
  --ST.spec
  -- E.spec
  -- C.spec
  -- T.spec
  LZ.spec
  --log "🍝"
