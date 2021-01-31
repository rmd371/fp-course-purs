
module Course.Gens where

import Course.List (List(..), hlist, listh, (:.))
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)
import Prelude (map, (<$>), (<*>), ($))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.QuickCheck (quickCheck)
import Prelude as P

import           Course.List         (List (..), hlist, listh)
import           Course.ListZipper   (ListZipper (..), zipper)

genList :: forall a. Arbitrary a => Gen (List a)
genList = map ((foldr (:.) Nil) :: Array a -> List a) arbitrary

-- shrinkList :: forall a. Arbitrary a => List a -> Array (List a)
-- shrinkList l = map listh $ shrink $ hlist l

genIntegerList :: Gen (List Int)
genIntegerList = genList

genIntegerAndList :: Gen (Tuple Int (List Int))
genIntegerAndList = map (map listh) arbitrary

-- -- shrinkIntegerAndList :: Integer -> List Integer -> Array (Tuple Integer (List Integer))
-- -- shrinkIntegerAndList = foldMap (foldMap listh) . shrink . foldMap hlist

genTwoLists :: Gen (Tuple (List Int) (List Int))
genTwoLists = Tuple <$> genIntegerList <*> genIntegerList

-- -- shrinkTwoLists :: List Integer -> List Integer -> Array (Tuple (List Integer) (List Integer))
-- -- shrinkTwoLists a b = P.fmap (\as bs -> (Tuple (listh as) (listh bs))) $ shrink (Tuple (hlist a) (hlist b))

genThreeLists :: Gen (Tuple3 (List Int) (List Int) (List Int))
genThreeLists = tuple3 <$> genIntegerList <*> genIntegerList <*> genIntegerList

-- -- shrinkThreeLists :: List Integer -> List Integer -> List Integer -> Array (Tuple3 (List Integer) (List Integer) (List Integer))
-- -- shrinkThreeLists a b c = foldMap (\as bs cs -> (Tuple3 (listh as) (listh bs) (listh cs)) $ shrink (hlist a, hlist b, hlist c)

genListOfLists :: Gen (List (List Int))
genListOfLists = map (map listh) (genList :: (Gen (List (Array Int))))

-- -- shrinkListOfLists :: Arbitrary a => List (List a) -> [List (List a)]
-- -- shrinkListOfLists = P.fmap (P.fmap listh). shrinkList . P.fmap hlist

-- -- forAllLists :: Testable prop => (List Int -> prop) -> Property
-- -- forAllLists = forAllShrink genIntegerList shrinkList

-- (List Integer) and a Bool
genListAndBool :: Gen (Tuple (List Int) Boolean)
genListAndBool = Tuple <$> genIntegerList <*> arbitrary

-- -- shrinkListAndBool :: (List Integer, Bool) -> [(List Integer, Bool)]
-- -- shrinkListAndBool (xs,b) = (,) P.<$> (shrinkList xs) P.<*> (shrink b)

-- -- forAllListsAndBool :: Testable prop
-- --                   => ((List Integer, Bool) -> prop)
-- --                   -> Property
-- -- forAllListsAndBool =
-- --   forAllShrink genListAndBool shrinkListAndBool

-- ListZipper Integer
genListZipper :: Gen (ListZipper Int)
genListZipper =
  zipper P.<$> arbitrary P.<*> arbitrary P.<*> arbitrary

-- shrinkListZipper :: ListZipper Int -> Array (ListZipper Int)
-- shrinkListZipper (ListZipper l x r) =
--   ListZipper P.<$> (shrinkList l) P.<*> (shrink x) P.<*> (shrinkList r)

-- forAllListZipper :: Testable prop
--                  => (ListZipper Int -> prop)
--                  -> Property
-- forAllListZipper =
--   forAllShrink genListZipper shrinkListZipper

-- -- genListZipperWithInt :: Gen (ListZipper Integer, Int)
-- -- genListZipperWithInt =
-- --   (,) P.<$> genListZipper P.<*> arbitrary

-- -- shrinkListZipperWithInt :: (ListZipper Integer, Int) -> [(ListZipper Integer, Int)]
-- -- shrinkListZipperWithInt (z, i) =
-- --   (,) P.<$> (shrinkListZipper z) P.<*> (shrink i)

-- -- forAllListZipperWithInt :: Testable prop
-- --                         => ((ListZipper Integer, Int) -> prop)
-- --                         -> Property
-- -- forAllListZipperWithInt =
-- --   forAllShrink genListZipperWithInt shrinkListZipperWithInt