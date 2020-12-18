module Test.ListSpec where

import Prelude

import Course.List (List(..), filter, find, flatMap, produceN, flatten, flattenAgain, foldLeft, headOr, hlist, infinity, largeList, length, lengthGT4, listh, product, reverse, seqOptional, sum, take, (:.))
import Course.Optional (Optional(..))
import Data.Array (length) as A
import Data.Int (even)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Testable, (<?>), (===))
import Test.QuickCheck (quickCheck) as Q
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

quickCheck :: forall prop. Testable prop => prop -> Aff Unit
quickCheck prop = liftEffect $ Q.quickCheck prop

listSpec :: Spec Unit
listSpec = describe "List" do
  describe "headOr" $ do
    it "headOr on non-empty list" $ headOr 3 (1 :. 2 :. Nil) `shouldEqual` 1
    it "headOr on empty list" $ headOr 3 Nil `shouldEqual` 3
    it "headOr on infinity always 0" $ quickCheck \x -> x `headOr` infinity == 0 <?> "Test failed for input " <> show x
    it "headOr on empty list always the default" $ quickCheck \x -> x `headOr` Nil === (x :: Int)

  describe "productTest" $ do
    it "product of empty list" $ product Nil `shouldEqual` 1
    it "product of 1..3" $ product (1 :. 2 :. 3 :. Nil) `shouldEqual` 6
    it "product of 1..4" $ product (1 :. 2 :. 3 :. 4 :. Nil) `shouldEqual` 24

  describe "sum" $ do
    it "sum 1..3" $ sum (1 :. 2 :. 3 :. Nil) `shouldEqual` 6
    it "sum 1..4" $ sum (1 :. 2 :. 3 :. 4 :. Nil) `shouldEqual` 10
    it "subtracting each element in a list from its sum is always 0" $
      quickCheck \x -> foldLeft (-) (sum x) x == 0 <?> "Test failed for input " <> show x

  describe "length" $ do
    it "length 1..3" $ length (1 :. 2 :. 3 :. Nil) `shouldEqual` 3
    it "summing a list of 1s is equal to its length" $ 
      quickCheck \x -> A.length (hlist x) == length (x :: List Int) <?> "Test failed for input " <> show x

  describe "filter" $ do
    it "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldEqual` (2 :. 4 :. Nil)
    it "filter (const True) is identity (headOr)" $ quickCheck
      \x -> headOr x (filter (const true) infinity) == 0 <?> "Test failed for input " <> show x
    it "filter (const True) is identity" $ quickCheck
      \x -> filter (const true) (x :: List Int) === x
    it "filter (const False) is the empty list" $ quickCheck
      \x -> filter (const false) (x :: List Int) == Nil <?> "Test failed for input " <> show x

  describe "map or <$>" $ do
    it "add 10 on list" $
      ((+) 10 <$> 1 :. 2 :. 3 :. Nil) `shouldEqual` (11 :. 12 :. 13 :. Nil)
    it "headOr after map" $ quickCheck
      \(x :: Int) -> headOr x ((+) 1 <$> infinity) == 1
    it "map id is id" $ quickCheck 
      \x -> (\y -> y) <$> x === (x :: List Int)

  describe "(<>)" $ do
    it "(1..6)" $
      ((1 :. 2 :. 3 :. Nil) <> (4 :. 5 :. 6 :. Nil)) `shouldEqual` listh [1,2,3,4,5,6]
    it "append empty to infinite" $ quickCheck
      \x -> headOr x (Nil <> infinity) == 0
    it "append anything to infinity" $ quickCheck
      \x y -> headOr x (y <> infinity) == headOr 0 y
    it "associativity" $ quickCheck
      \x y (z :: List Int) -> (x <> y) <> z == x <> (y <> z)
    it "append to empty list" $ quickCheck
      \(x :: List Int) -> x <> Nil == x

  describe "flatten" $ do
    it "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) `shouldEqual` listh [1,2,3,4,5,6,7,8,9]
    it "flatten (infinity :. y)" $ quickCheck
      \x y -> headOr x (flatten (infinity :. y :. Nil)) == 0
    it "flatten (y :. infinity)" $ quickCheck
      \x y -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
    it "sum of lengths == length of flattened" $ quickCheck
      \(x :: List (List Int)) -> sum (map length x) == length (flatten x) <?> "Test failed for input " <> show x

  describe "flatMap" $ do
    it "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) `shouldEqual` listh [1,2,3,2,3,4,3,4,5]
    it "flatMap id flattens a list of lists" $ quickCheck
      \x y -> headOr x (flatMap identity (infinity :. y :. Nil)) == 0
    it "flatMap id on a list of lists take 2" $ quickCheck
      \x y -> headOr x (flatMap identity (y :. infinity :. Nil)) == headOr 0 y
    it "flatMap id == flatten" $ quickCheck
      \(x :: List (List Int)) -> flatMap identity x == flatten x

  describe "flattenAgain" $ do
    it "lists of Integer" $ quickCheck
      \(x :: List (List Int)) -> flatten x == flattenAgain x

  describe "seqOptional" $ do
    it "all Full" $
      seqOptional (Full 1 :. Full 10 :. Nil) `shouldEqual` Full (1 :. 10 :. Nil)
    it "empty list" $
      let empty = Nil :: List (Optional Int)
       in seqOptional empty `shouldEqual` Full Nil
    it "contains Empty" $
      seqOptional (Full 1 :. Full 10 :. Empty :. Nil) `shouldEqual` Empty
    it "Empty at head of infinity" $
      seqOptional (Empty :. map Full infinity) `shouldEqual` Empty

  describe "find" $ do
    it "find no matches" $
      find even (1 :. 3 :. 5 :. Nil) `shouldEqual` Empty
    it "empty list" $ find even Nil `shouldEqual` Empty
    it "find only even" $
      find even (1 :. 2 :. 3 :. 5 :. Nil) `shouldEqual` Full 2
    it "find first, not second even" $
      find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldEqual` Full 2
    it "find on infinite list" $
      find (const true) infinity `shouldEqual` Full 0

  describe "lengthGT4" $ do
    it "list of length 3" $
      lengthGT4 (1 :. 3 :. 5 :. Nil) `shouldEqual` false
    it "empty list" $
      lengthGT4 Nil `shouldEqual` false
    it "list of length 5" $
      lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldEqual` true
    it "infinite list" $
      lengthGT4 infinity `shouldEqual` true

  describe "reverse" $ do
    it "empty list" $
      reverse Nil `shouldEqual` (Nil :: List Int)
    it "reverse on one element list should be itself" $
      reverse (1 :. Nil) `shouldEqual` (1 :. Nil)
    it "reverse on [3,2,1] should equal [1,2,3]" $
      reverse (3 :. 2 :. 1 :. Nil) `shouldEqual` (1 :. 2 :. 3 :. Nil)
    it "reverse . reverse on largeList" $
      take 1 (reverse (reverse largeList)) `shouldEqual` (1 :. Nil)
    it "reverse then append is same as append then reverse" $ quickCheck
      \x (y :: List Int) -> reverse x <> reverse y == reverse (y <> x)
    -- prop "" $
    --   forAllLists (\x -> reverse (x :. Nil) == x :. Nil)

  describe "produce" $ do
    it "increment" $
      produceN ((+) 1) 0 4 `shouldEqual` (0:.1:.2:.3:.Nil)
    it "double" $
      produceN ((*) 2) 1 4 `shouldEqual` (1:.2:.4:.8:.Nil)