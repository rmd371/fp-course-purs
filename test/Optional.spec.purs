module Test.OptionalSpec where

import Prelude

import Course.List (foldLeft)
import Course.Optional (Optional(..), mapOptional, bindOptional, (??), (<+>))
import Data.Int (even)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.QuickCheck (quickCheck, quickCheckGen, (<?>), (>?))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

optionalSpec :: Effect Unit
optionalSpec = launchAff_ $ runSpec [consoleReporter] do
  describe "mapOptional" $ do
    it "Empty" $
      mapOptional (add 1) Empty `shouldEqual` Empty
    it "Full" $
      mapOptional (add 1) (Full 8) `shouldEqual` Full 9

  let
    evenDecOddInc n =
      if even n
      then Full (n - 1)
      else Full (n + 1)

  describe "bindOptional" $ do
    it "Empty" $
      bindOptional Full Empty `shouldEqual` (Empty :: Optional Int)
    it "even dec, odd inc, even input" $
      bindOptional evenDecOddInc (Full 8) `shouldEqual` Full 7
    it "even dec, odd inc, odd input" $
      bindOptional evenDecOddInc (Full 9) `shouldEqual` Full 10

  describe "??" $ do
    it "Full" $
      (Full 8 ?? 99) `shouldEqual` 8
    it "Empty" $
      (Empty ?? 99) `shouldEqual` 99

  describe "<+>" $ do
    it "first Full" $
      (Full 8 <+> Empty) `shouldEqual` Full 8
    it "both Full" $
      (Full 8 <+> Full 9) `shouldEqual` Full 8
    it "first Empty" $
      (Empty <+> Full 9) `shouldEqual` Full 9
    it "both empty" $
      (Empty <+> Empty) `shouldEqual` (Empty :: Optional Int)
--     it "quickcheck test" $
--       liftEffect $ quickCheck \x -> myFunc x > x <?> "Test failed for input " <> show x
--     it "quickcheck test2" $
--       liftEffect $ quickCheck \x -> (myFunc x) >? x
--     it "subtracting each element in a list from its sum is always 0" $
--       liftEffect $ quickCheck (\x -> foldLeft (-) (sum x) x == 0)

-- myFunc :: Int -> Int
-- myFunc x = x + 1