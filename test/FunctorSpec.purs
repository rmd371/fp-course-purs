module Course.FunctorSpec where

import Prelude (discard, Unit, unit, ($), (+), (*), (==))

import Course.ExactlyOne (ExactlyOne(..))
import Course.Functor (void, (<$), (<$>))
import Course.List (List(..), (:.))
import Course.Optional (Optional(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Testable)
import Test.QuickCheck (quickCheck) as Q
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

quickCheck :: forall prop. Testable prop => prop -> Aff Unit
quickCheck prop = liftEffect $ Q.quickCheck prop

functorSpec :: Spec Unit
functorSpec = do
  describe "ExactlyOne" $ do
    it "ExactlyOne" $ (((+) 1) <$> ExactlyOne 2) `shouldEqual` ExactlyOne 3

  describe "List" $ do
    it "empty list" $
      (((+) 1) <$> Nil) `shouldEqual` Nil
    it "increment" $
      (((+) 1) <$> (1 :. 2 :. 3 :. Nil)) `shouldEqual` (2 :. 3 :. 4 :. Nil)

  describe "Optional" $ do
    it "Empty" $ (((+) 1) <$> Empty) `shouldEqual` Empty
    it "Full"  $ (((+) 1) <$> Full 2) `shouldEqual` Full 3

  describe "Function" $ do
    it "(->)" $ (((+) 1) <$> ((*) 2)) 8 `shouldEqual` 17

  describe "(<$)" $ do
    it "Map 7" $ (7 <$ (1 :. 2 :. 3 :. Nil)) `shouldEqual` (7 :. 7 :. 7 :. Nil)
    it "Always maps a constant value over List" $ quickCheck
      \(x :: Int) a b c -> (x <$ ((a :. b :. c :. Nil) :: List Int)) == (x :. x :. x :. Nil)
    it "Always maps a constant value over Full (Optional)" $ quickCheck
      \(x :: Int) (q :: Int) -> (x <$ Full q) == Full x

  describe "void" $ do
    it "List"  $ void (1 :. 2 :. 3 :. Nil) `shouldEqual` (unit :. unit :. unit :. Nil)
    it "Full"  $ void (Full 7) `shouldEqual` Full unit
    it "Empty" $ void Empty `shouldEqual` Empty
    it "(->)"  $ void ((+) 10) 5 `shouldEqual` unit