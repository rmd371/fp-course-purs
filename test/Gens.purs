
module Course.Gens where


import Course.Core
import Prelude

import Data.Foldable (foldMap)

-- import           Course.List         (List (..), hlist, listh)
-- import           Course.ListZipper   (ListZipper (..), zipper)

-- genList :: Arbitrary a => Gen (List a)
-- genList = P.fmap ((P.foldr (:.) Nil) :: Array a -> List a) arbitrary

-- shrinkList :: Arbitrary a => List a -> Array (List a)
-- shrinkList = foldMap listh . shrink . hlist

-- genIntegerList :: Gen (List Integer)
-- genIntegerList = genList

-- genIntegerAndList :: Gen (Tuple (Integer (List Integer)))
-- genIntegerAndList = foldMap (P.fmap listh) arbitrary

-- shrinkIntegerAndList :: Integer -> List Integer -> Array (Tuple Integer (List Integer))
-- shrinkIntegerAndList = foldMap (foldMap listh) . shrink . foldMap hlist

-- genTwoLists :: Gen (Tuple (List Integer) (List Integer))
-- genTwoLists = (,) P.<$> genIntegerList P.<*> genIntegerList

-- shrinkTwoLists :: List Integer -> List Integer -> Array (Tuple (List Integer) (List Integer))
-- shrinkTwoLists a b = P.fmap (\as bs -> (Tuple (listh as) (listh bs))) $ shrink (Tuple (hlist a) (hlist b))

-- genThreeLists :: Gen (List Integer, List Integer, List Integer)
-- genThreeLists = (,,) P.<$> genIntegerList P.<*> genIntegerList P.<*> genIntegerList

-- shrinkThreeLists :: List Integer -> List Integer -> List Integer -> Array (Tuple3 (List Integer) (List Integer) (List Integer))
-- shrinkThreeLists a b c = foldMap (\as bs cs -> (Tuple3 (listh as) (listh bs) (listh cs)) $ shrink (hlist a, hlist b, hlist c)

-- genListOfLists :: Gen (List (List Integer))
-- genListOfLists = P.fmap (P.fmap listh) (genList :: (Gen (List [Integer])))

-- shrinkListOfLists :: Arbitrary a => List (List a) -> [List (List a)]
-- shrinkListOfLists = P.fmap (P.fmap listh). shrinkList . P.fmap hlist

-- forAllLists :: Testable prop => (List Integer -> prop) -> Property
-- forAllLists = forAllShrink genIntegerList shrinkList

-- -- (List Integer) and a Bool
-- genListAndBool :: Gen (List Integer, Bool)
-- genListAndBool = (,) P.<$> genIntegerList P.<*> arbitrary

-- shrinkListAndBool :: (List Integer, Bool) -> [(List Integer, Bool)]
-- shrinkListAndBool (xs,b) = (,) P.<$> (shrinkList xs) P.<*> (shrink b)

-- forAllListsAndBool :: Testable prop
--                   => ((List Integer, Bool) -> prop)
--                   -> Property
-- forAllListsAndBool =
--   forAllShrink genListAndBool shrinkListAndBool

-- -- ListZipper Integer
-- genListZipper :: Gen (ListZipper Integer)
-- genListZipper =
--   zipper P.<$> arbitrary P.<*> arbitrary P.<*> arbitrary

-- shrinkListZipper :: ListZipper Integer -> [ListZipper Integer]
-- shrinkListZipper (ListZipper l x r) =
--   ListZipper P.<$> (shrinkList l) P.<*> (shrink x) P.<*> (shrinkList r)

-- forAllListZipper :: Testable prop
--                  => (ListZipper Integer -> prop)
--                  -> Property
-- forAllListZipper =
--   forAllShrink genListZipper shrinkListZipper

-- genListZipperWithInt :: Gen (ListZipper Integer, Int)
-- genListZipperWithInt =
--   (,) P.<$> genListZipper P.<*> arbitrary

-- shrinkListZipperWithInt :: (ListZipper Integer, Int) -> [(ListZipper Integer, Int)]
-- shrinkListZipperWithInt (z, i) =
--   (,) P.<$> (shrinkListZipper z) P.<*> (shrink i)

-- forAllListZipperWithInt :: Testable prop
--                         => ((ListZipper Integer, Int) -> prop)
--                         -> Property
-- forAllListZipperWithInt =
--   forAllShrink genListZipperWithInt shrinkListZipperWithInt