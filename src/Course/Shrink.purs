module Course.Shrink where

-- import Prelude

-- import Control.Monad.List.Trans (iterate)
-- import Course.List (List(..), (:.))
-- import Course.List as List
-- import Data.Array (concatMap, drop, length, take, takeWhile, (:))

-- -- | Produce a smaller permutation of the input list.
-- shrinkList :: forall a. Array a -> Array (Array a)
-- shrinkList xs = do
--  concatMap
--    (\k -> removes k xs)
--    (halves $ List.length xs)

-- -- | Produces a list containing the results of halving a number over and over
-- -- | again.
-- -- |
-- -- | > halves 30 == [30,15,7,3,1]
-- -- | > halves 128 == [128,64,32,16,8,4,2,1]
-- -- | > halves (-10) == [-10,-5,-2,-1]
-- -- |
-- halves :: forall a. Ord a => EuclideanRing a => a -> Array a
-- halves =
--   let
--     two =
--       one + one
--   in
--     takeWhile (\x -> x /= zero) <<<
--     iterate (\x -> x `div` two)

-- -- | Permutes a list by removing 'k' consecutive elements from it:
-- -- |
-- -- | > removes 2 [1,2,3,4,5,6] == [[3,4,5,6],[1,2,5,6],[1,2,3,4]]
-- -- |
-- removes :: forall a. Int -> Array a -> Array (Array a)
-- removes k0 xs0 =
--   let
--     loop :: Int -> Int -> Array a -> List (List a)
--     loop k n xs =
--       let
--         hd = take k xs
--         tl = drop k xs
--       in
--         if k > n then
--           List.Nil
--         else if length tl == 0 then
--           List.Nil
--         else
--           tl (:) (map (\x -> hd <> x) (loop k (n - k) tl))
--   in
--     loop k0 (List.length xs0) xs0