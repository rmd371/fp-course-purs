module Course.ExtendSpec where

import Prelude (($), discard, identity, (<<<), Unit)
import Course.ExactlyOne (ExactlyOne(ExactlyOne))
import Course.Extend (cojoin, (<<=))
import Course.Functor ((<$>))
import Course.List (List(..), length, listh, reverse, (:.))
import Course.Optional (Optional(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "ExactlyOne instance" $
    (identity <<= ExactlyOne 7) `shouldEqual` ExactlyOne (ExactlyOne 7)

  describe "List" $ do
    it "length" $
      (length <<= ('a' :. 'b' :. 'c' :. Nil)) `shouldEqual` (3 :. 2 :. 1 :. Nil)
    it "id" $
      (identity <<= (1 :. 2 :. 3 :. 4 :. Nil)) `shouldEqual` nestedListh2 [[1,2,3,4],[2,3,4],[3,4],[4]]
    it "reverse" $
      (reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)) `shouldEqual`
        nestedListh3 [[[4,5,6],[1,2,3]],[[4,5,6]]]

  describe "Optional" $ do
    it "id Full" $
      (identity <<= (Full 7)) `shouldEqual` Full (Full 7)
    it "id Empty" $
      (identity <<= Empty) `shouldEqual` (Empty :: Optional (Optional Int))

  describe "cojoin" $ do
    it "ExactlyOne" $
      cojoin (ExactlyOne 7) `shouldEqual` ExactlyOne (ExactlyOne 7)
    it "List" $
      cojoin (1 :. 2 :. 3 :. 4 :. Nil) `shouldEqual` nestedListh2 [[1,2,3,4],[2,3,4],[3,4],[4]]
    it "Full" $
      cojoin (Full 7) `shouldEqual` Full (Full 7)
    it "Empty" $
      cojoin Empty `shouldEqual` (Empty :: Optional (Optional Int))


nestedListh2 :: forall a. Array (Array a) -> List (List a)
nestedListh2 = ((<$>) listh) <<< listh

nestedListh3 :: forall a. Array (Array (Array a)) -> List (List (List a))
nestedListh3 = ((<$>) ((<$>) listh)) <<< nestedListh2