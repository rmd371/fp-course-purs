module Course.ListSpec where

import Course.Core
import Prelude

import Course.List (List(..), filter, find, flatMap, flatten, headOr, infinity, length, listh, product, reverse, sum, (:.))
import Course.Optional (Optional(..))
import Data.Int (even)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

spec :: Effect Unit
spec = launchAff_ $ runSpec [consoleReporter] do
  describe "headOr" $ do
    it "headOr on non-empty list" $ headOr 3 (1 :. 2 :. Nil) `shouldEqual` 1
    it "headOr on empty list" $ headOr 3 Nil `shouldEqual` 3
    it "headOr on infinity always 0" $ quickCheck \x -> x `headOr` infinity == 0
    it "headOr on empty list always the default" $ quickCheck \x -> x `headOr` Nil == (x :: Int)

  describe "productTest" $ do
    it "product of empty list" $ product Nil `shouldEqual` 1
    it "product of 1..3" $ product (1 :. 2 :. 3 :. Nil) `shouldEqual` 6
    it "product of 1..4" $ product (1 :. 2 :. 3 :. 4 :. Nil) `shouldEqual` 24

  describe "sum" $ do
    it "sum 1..3" $ sum (1 :. 2 :. 3 :. Nil) `shouldEqual` 6
    it "sum 1..4" $ sum (1 :. 2 :. 3 :. 4 :. Nil) `shouldEqual` 10
    -- it "subtracting each element in a list from its sum is always 0" $ quickCheck $
    --   forAllShrink genList shrinkList (\x -> foldLeft (-) (sum x) x == 0)

  describe "length" $ do
    it "length 1..3" $ length (1 :. 2 :. 3 :. Nil) `shouldEqual` 3
  --   prop "summing a list of 1s is equal to its length" $
  --     forAllLists (\x -> P.length (hlist x) == length x)

  describe "map" $ do
    it "add 10 on list" $
      map ((+) 10) (1 :. 2 :. 3 :. Nil) `shouldEqual` (11 :. 12 :. 13 :. Nil)
    it "headOr after map" $ quickCheck
      \x -> headOr (x :: Int) (map ((+) 1) infinity) == 1
  --   prop "map id is id" $
  --     forAllLists (\x -> map id x == x)

  describe "filter" $ do
    it "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldEqual` (2 :. 4 :. Nil)
    it "filter (const True) is identity (headOr)" $ quickCheck
      \x -> headOr x (filter (const true) infinity) == 0
  --   prop "filter (const True) is identity" $
  --     forAllLists (\x -> filter (const True) x == x)
  --   prop "filter (const False) is the empty list" $
  --     forAllLists (\x -> filter (const False) x == Nil)

  describe "(++)" $ do
    it "(1..6)" $
      ((1 :. 2 :. 3 :. Nil) <> (4 :. 5 :. 6 :. Nil)) `shouldEqual` listh [1,2,3,4,5,6]
    it "append empty to infinite" $ quickCheck
      \x -> headOr x (Nil <> infinity) == 0
  --   prop "append anything to infinity" $
  --      forAllShrink genIntegerAndList shrinkIntegerAndList (\x y -> headOr x (y ++ infinity) == headOr 0 y)
  --   prop "associativity" $
  --     forAllShrink genThreeLists shrinkThreeLists (\x y z -> (x ++ y) ++ z == x ++ (y ++ z))
  --   prop "append to empty list" $
  --     forAllLists (\x -> x ++ Nil == x)

  describe "flatten" $ do
    it "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) `shouldEqual` listh [1,2,3,4,5,6,7,8,9]
  --   prop "flatten (infinity :. y)" $
  --     forAllShrink genIntegerAndList shrinkIntegerAndList (\x y -> headOr x (flatten (infinity :. y :. Nil)) == 0)
  --   prop "flatten (y :. infinity)" $
  --     forAllShrink genIntegerAndList shrinkIntegerAndList (\x y -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y)
  --   prop "sum of lengths == length of flattened" $
  --     forAllShrink genListOfLists shrinkListOfLists (\x -> sum (map length x) == length (flatten x))

  describe "flatMap" $ do
    it "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) `shouldEqual` listh [1,2,3,2,3,4,3,4,5]
  --   prop "flatMap id flattens a list of lists" $
  --     forAllShrink genIntegerAndList shrinkIntegerAndList (\x y -> headOr x (flatMap id (infinity :. y :. Nil)) == 0)
  --   prop "flatMap id on a list of lists take 2" $
  --     forAllShrink genIntegerAndList shrinkIntegerAndList (\x y -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y)
  --   prop "flatMap id == flatten" $
  --     forAllShrink genListOfLists shrinkListOfLists (\x -> flatMap id x == flatten x)

  -- describe "flattenAgain" $ do
  --   prop "lists of Integer" $
  --     forAllShrink genListOfLists shrinkListOfLists (\x -> flatten x == flattenAgain x)

  -- describe "seqOptional" $ do
  --   it "all Full" $
  --     seqOptional (Full 1 :. Full 10 :. Nil) `shouldEqual` Full (1 :. 10 :. Nil)
  --   it "empty list" $
  --     let empty = Nil :: List (Optional Integer)
  --      in seqOptional empty `shouldEqual` Full Nil
  --   it "contains Empty" $
  --     seqOptional (Full 1 :. Full 10 :. Empty :. Nil) `shouldEqual` Empty
  --   it "Empty at head of infinity" $
  --     seqOptional (Empty :. map Full infinity) `shouldEqual` Empty

  -- describe "find" $ do
  --   it "find no matches" $
  --     find even (1 :. 3 :. 5 :. Nil) `shouldEqual` Empty
  --   it "empty list" $ find even Nil `shouldEqual` Empty
  --   it "find only even" $
  --     find even (1 :. 2 :. 3 :. 5 :. Nil) `shouldEqual` Full 2
  --   it "find first, not second even" $
  --     find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldEqual` Full 2
  --   it "find on infinite list" $
  --     find (const True) infinity `shouldEqual` Full 0

  -- describe "lengthGT4" $ do
  --   it "list of length 3" $
  --     lengthGT4 (1 :. 3 :. 5 :. Nil) `shouldEqual` False
  --   it "empty list" $
  --     lengthGT4 Nil `shouldEqual` False
  --   it "list of length 5" $
  --     lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldEqual` True
  --   it "infinite list" $
  --     lengthGT4 infinity `shouldEqual` True

  -- describe "reverse" $ do
  --   it "empty list" $
  --     reverse Nil `shouldEqual` (Nil :: List Integer)
  --   it "reverse . reverse on largeList" $
  --     take 1 (reverse (reverse largeList)) `shouldEqual` (1 :. Nil)
  --   prop "reverse then append is same as append then reverse" $
  --     forAllShrink genTwoLists shrinkTwoLists (\x y -> reverse x ++ reverse y == reverse (y ++ x))
  --   prop "" $
  --     forAllLists (\x -> reverse (x :. Nil) == x :. Nil)

  -- describe "produce" $ do
  --   it "increment" $
  --     let (x:.y:.z:.w:._) = produce ((+) 1) 0
  --      in (x:.y:.z:.w:.Nil) `shouldEqual` (0:.1:.2:.3:.Nil)
  --   it "double" $
  --     let (x:.y:.z:.w:._) = produce ((*) 2) 1
  --      in (x:.y:.z:.w:.Nil) `shouldEqual` (1:.2:.4:.8:.Nil)