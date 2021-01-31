module Course.TraversableSpec where

import Course.ExactlyOne (ExactlyOne(..))
import Course.List (List(..), (:.))
import Course.Optional (Optional(..))
import Course.Traversable (sequenceA)
import Prelude (Unit, discard, ($), (*))
import Test.Spec (describe, Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "sequenceA" $ do
    it "ExactlyOne/List" $ sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil) `shouldEqual` ExactlyOne (7 :. 8 :. 9 :. Nil)

    it "ExactlyOne/Optional" $
      sequenceA (Full (ExactlyOne 7)) `shouldEqual` ExactlyOne (Full 7)

    it "Optional/Function" $
      sequenceA (Full ((*) 10)) 6 `shouldEqual` Full 60
