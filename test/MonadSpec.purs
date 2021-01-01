module Course.MonadSpec where

import Course.ExactlyOne (ExactlyOne(..))
import Course.List (List(..), (:.))
import Course.Monad (join, (<**>), (<=<), (=<<), (>>=))
import Course.Optional (Optional(..))
import Prelude (discard, Unit, ($), (+), (*))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Bind tests" $ do
    it "(=<<) for ExactlyOne" $
      ((\x -> ExactlyOne (x+1)) =<< ExactlyOne 2) `shouldEqual` (ExactlyOne 3)

    it "(=<<) for List" $
      ((\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)) `shouldEqual` (1:.1:.2:.2:.3:.3:.Nil)

    it "(=<<) for Optional" $
      ((\n -> Full (n + n)) =<< Full 7) `shouldEqual` Full 14

    it "(=<<) for (->)" $
      (((*) =<< ((+) 10)) 7) `shouldEqual` 119

  describe "<**>" $ do
    it "ExactlyOne" $
      (ExactlyOne ((+) 10) <**> ExactlyOne 8) `shouldEqual` ExactlyOne 18
    it "List" $
      (((+) 1) :. ((*)2) :. Nil <**> 1 :. 2 :. 3 :. Nil) `shouldEqual` (2:.3:.4:.2:.4:.6:.Nil)
    it "Optional" $
      (Full ((+)8) <**> Full 7) `shouldEqual` Full 15
    it "Optional - empty function" $
      (Empty <**> Full 7) `shouldEqual` (Empty :: Optional Int)
    it "Optional - empty value" $
      (Full ((+)8) <**> Empty) `shouldEqual` Empty
    it "(->) 1" $
      ((+) <**> ((+)10)) 3 `shouldEqual` 16
    it "(->) 2" $
      ((+) <**> ((+)5)) 3 `shouldEqual` 11
    it "(->) 3" $
      ((+) <**> ((+)5)) 1 `shouldEqual` 7
    it "(->) 4" $
      ((*) <**> ((+)10)) 3 `shouldEqual` 39
    it "(->) 5" $
      ((*) <**> ((+)2)) 3 `shouldEqual` 15

  describe "join" $ do
    it "List" $
      join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil) `shouldEqual` (1:.2:.3:.1:.2:.Nil)
    it "Optional with Empty" $
      join (Full Empty) `shouldEqual` (Empty :: Optional Int)
    it "Optional all Full" $
      join (Full (Full 7)) `shouldEqual` Full 7
    it "(->)" $
      join (+) 7 `shouldEqual` 14

  describe "bindFlipped" $ do
    it "(>>=)" $
      (((+)10) >>= (*)) 7 `shouldEqual` 119

  describe "Kleisli Composition" $ do
    it "kleisliComposition" $
      ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
        `shouldEqual`
          (2:.2:.3:.3:.Nil)
