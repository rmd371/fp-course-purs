-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (unsafeThrow "todo: ...") with an appropriate
--   solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import Course.Optional
import Prelude

import Data.Array (concat, foldl, foldr, (..), (:))
import Data.Array.NonEmpty (concatMap)
import Data.Lazy (defer, force)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import Logger (error)
import Pipes.Prelude (seq)
import Prelude as P
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, sized)

-- import qualified System.Environment as E
-- import qualified Prelude as P
-- import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap ((P.foldr (:.) Nil) :: ([a] -> List a)) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List a = Nil | Cons a (List a)

derive instance listEq :: Eq a => Eq (List a)
derive instance listOrd :: Ord a => Ord (List a)

-- Right-associative
infixr 5 Cons as :.

instance showList :: Show t => Show (List t) where
  show = show <<< foldRight (:) []

-- The list of integers from zero to infinity.
infinity :: List Int
infinity = listh (0..1000)
  -- let inf x = if x < 10000 then x :. inf (x+1) else x :. Nil
  -- in inf 0

-- functions over List that you may consider using
foldRight :: forall a b. (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

-- TODO: get this working!!!!
-- foldLeft :: forall a b. (b -> a -> b) -> b -> List a -> b
-- foldLeft _ b Nil      = b
-- foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t
foldLeft :: forall a b. (b -> a -> b) -> b -> List a -> b
foldLeft f b l = foldl f b (hlist l)
-- END Helper functions and data types

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
headOr :: forall a. a -> List a -> a
headOr a Nil = a
headOr _ (a :. _) = a
--headOr = unsafeThrow "todo: Course.List#headOr"

-- | The product of the elements of a list.
--
-- >>> product Nil
-- 1
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product :: List Int -> Int
product Nil = 1
product (a :. as) = a * product as

-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> \x -> foldLeft (-) (sum x) x == 0
sum :: List Int -> Int
sum Nil = 0
sum (a :. as) = a + (sum as)

-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> \x -> sum (map (const 1) x) == length x
length :: forall a. List a -> Int
length Nil = 0
length (a :. as) = 1 + length as

-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> \x -> headOr x (filter (const True) infinity) == 0
--
-- prop> \x -> filter (const True) x == x
--
-- prop> \x -> filter (const False) x == Nil
filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter f (a :. as) = 
  if   f a 
  then a :. (filter f as) 
  else filter f as 

-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> \x -> headOr x (map (+1) infinity) == 1
--
-- prop> \x -> map id x == x
instance functorList :: Functor List where
  map :: forall a b. (a -> b) -> List a -> List b
  map _ Nil = Nil
  map f (a :. as) = f a :. map f as

-- -- | Append two lists to a new list.
-- --
-- -- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- -- [1,2,3,4,5,6]
-- --
-- -- prop> \x -> headOr x (Nil ++ infinity) == 0
-- --
-- -- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
-- --
-- -- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
-- --
-- -- prop> \x -> x ++ Nil == x
instance semigroupList :: Semigroup (List a) where
  append :: forall a. List a -> List a -> List a
  append Nil       as' = as'
  append (a :. as) as' = a :. append as as'

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> sum (map length x) == length (flatten x)
flatten :: forall a. List (List a) -> List a
flatten Nil          = Nil
flatten (as :. list) = as <> flatten list

-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> flatMap id (x :: List (List Int)) == flatten x
flatMap :: forall a b. (a -> List b) -> List a -> List b
flatMap _ Nil       = Nil
flatMap f (a :. as) = f a <> flatMap f as

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain :: forall a. List (List a) -> List a
flattenAgain list = flatMap identity list

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values, 
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional :: forall a. List (Optional a) -> Optional (List a)
seqOptional = \x -> Empty -- TODO: Course.List#seqOptional

-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find :: forall a. (a -> Boolean) -> List a -> Optional a
find = \x y -> Empty -- TODO: Course.List#find

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 :: forall a. List a -> Boolean
lengthGT4 = \x -> false -- TODO: Course.List#lengthGT4

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse :: forall a. List a -> List a
reverse = \x -> Nil -- TODO: Course.List#reverse

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce :: forall a. (a -> a) -> a -> List a
produce f x = x :. produce f (f x)

-- -- | Do anything other than reverse a list.
-- -- Is it even possible?
-- --
-- -- >>> notReverse Nil
-- -- []
-- --
-- -- prop> \x -> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
-- --
-- -- prop> \x -> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
-- notReverse :: forall a. 
--   List a
--   -> List a
-- notReverse =
--   unsafeThrow "todo: Is it even possible?"

-- ---- End of list exercises

largeList :: List Int
largeList = listh (1..50000)

hlist :: forall a. List a -> Array a
hlist = foldRight (:) []

listh :: forall a. Array a -> List a
listh = foldr (:.) Nil

-- RMD added code for quickcheck
instance arbitraryList :: Arbitrary a => Arbitrary (List a) where
  arbitrary = map listh arbitrary
-- RMD added code for quickcheck

-- putStr :: Chars -> Effect ()
-- putStr = log <<< hlist

-- putStrLn :: Chars -> Effect Unit
-- putStrLn = log <<< hlist

-- readFile :: FilePath -> Aff Chars
-- readFile = map listh <<< readFile <<< hlist

-- writeFile ::
--   FilePath
--   -> Chars
--   -> Aff ()
-- writeFile n s =
--   P.writeFile (hlist n) (hlist s)

-- getLine ::
--   Aff Chars
-- getLine =
--   P.fmap listh P.getLine

-- getArgs ::
--   Aff (List Chars)
-- getArgs =
--   P.fmap (listh . fmap listh) E.getArgs

-- isPrefixOf :: forall a. 
--   Eq a =>
--   List a
--   -> List a
--   -> Boolean
-- isPrefixOf Nil _ =
--   true
-- isPrefixOf _  Nil =
--   false
-- isPrefixOf (x:.xs) (y:.ys) =
--   x == y && isPrefixOf xs ys

-- isEmpty :: forall a. 
--   List a
--   -> Boolean
-- isEmpty Nil =
--   true
-- isEmpty (_:._) =
--   false

-- span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
-- span p x = Tuple (takeWhile p x) (dropWhile p x)

-- break :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
-- break p = span (not <<< p)

-- dropWhile :: forall a. 
--   (a -> Boolean)
--   -> List a
--   -> List a
-- dropWhile _ Nil =
--   Nil
-- dropWhile p xs@(x:.xs') =
--   if p x
--     then
--       dropWhile p xs'
--     else
--       xs

-- takeWhile :: forall a. 
--   (a -> Boolean)
--   -> List a
--   -> List a
-- takeWhile _ Nil =
--   Nil
-- takeWhile p (x:.xs) =
--   if p x
--     then
--       x :. takeWhile p xs
--     else
--       Nil

-- zip :: forall a b. List a -> List b -> List (Tuple a b)
-- zip = zipWith Tuple

-- zipWith :: forall a b c.
--   (a -> b -> c)
--   -> List a
--   -> List b
--   -> List c
-- zipWith f (a:.as) (b:.bs) =
--   f a b :. zipWith f as bs
-- zipWith _ _  _ =
--   Nil

-- unfoldr :: forall a b. 
--   (a -> Optional (Tuple b a))
--   -> a
--   -> List b
-- unfoldr f b  =
--   case f b of
--     Full (Tuple a z) -> a :. unfoldr f z
--     Empty -> Nil

-- lines ::
--   Chars
--   -> List Chars
-- lines =
--   listh . fmap listh . lines . hlist

-- unlines ::
--   List Chars
--   -> Chars
-- unlines =
--   listh . unlines . hlist . map hlist

-- words :: Chars -> List Chars
-- words = listh <<< (map listh) <<< words <<< hlist

-- unwords ::
--   List Chars
--   -> Chars
-- unwords =
--   listh . unwords . hlist . map hlist

-- listOptional :: forall a b. 
--   (a -> Optional b)
--   -> List a
--   -> List b
-- listOptional _ Nil =
--   Nil
-- listOptional f (h:.t) =
--   let r = listOptional f t
--   in case f h of
--        Empty -> r
--        Full q -> q :. r

-- any :: forall a. 
--   (a -> Boolean)
--   -> List a
--   -> Boolean
-- any p =
--   foldRight ((||) . p) false

-- all :: forall a. 
--   (a -> Boolean)
--   -> List a
--   -> Boolean
-- all p =
--   foldRight ((&&) . p) true

-- or :: List Boolean -> Boolean
-- or = any identity

-- and ::
--   List Boolean
--   -> Boolean
-- and =
--   all identity

-- elem :: forall a. Eq a => a -> List a -> Boolean
-- elem x = any (eq x)

-- notElem :: forall a. 
--   Eq a =>
--   a
--   -> List a
--   -> Boolean
-- notElem x =
--   all (notEq x)

-- -- permutations
-- --   :: List a -> List (List a)
-- -- permutations xs0 =
-- --   let perms Nil _ =
-- --         Nil
-- --       perms (t:.ts) is =
-- --         let interleave' _ Nil r = (Tuple ts r)
-- --             interleave' f (y:.ys) r =
-- --                let (Tuple us zs) = interleave' (f <<< (y:.)) ys r
-- --                in  (y:.us, f (t:.y:.us):.zs)
-- --         in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
-- --   in xs0 :. perms xs0 Nil

-- intersectBy :: forall a b. 
--   (a -> b -> Boolean)
--   -> List a
--   -> List b
--   -> List a
-- intersectBy e xs ys =
--   filter (\x -> any (e x) ys) xs

take :: forall a. Int -> List a -> List a
take n _ | n <= 0 = Nil
take _ Nil = Nil
take n (x:.xs) = x :. take (n - 1) xs

-- drop :: forall a. Int -> List a -> List a
-- drop n xs | n <= 0 =
--   xs
-- drop _ Nil =
--   Nil
-- drop n (_:.xs) =
--   drop (n-1) xs

-- repeat :: forall a. 
--   a
--   -> List a
-- repeat x =
--   x :. repeat x

-- replicate :: forall a. Int -> a -> List a
-- replicate n x =
--   take n (repeat x)

-- -- reads ::
-- --   P.Read a =>
-- --   Chars
-- --   -> Optional (Tuple a Chars)
-- -- reads s =
-- --   case P.reads (hlist s) of
-- --     [] -> Empty
-- --     ((Tuple a q):_) -> Full (Tuple a (listh q))

-- -- read ::
-- --   P.Read a =>
-- --   Chars
-- --   -> Optional a
-- -- read =
-- --   mapOptional fst . reads

-- -- readHexs ::
-- --   (Tuple (Eq a) (Num a)) =>
-- --   Chars
-- --   -> Optional (Tuple a Chars)
-- -- readHexs s =
-- --   case N.readHex (hlist s) of
-- --     [] -> Empty
-- --     ((Tuple a q):_) -> Full (Tuple a (listh q))

-- -- readHex ::
-- --   (Tuple (Eq a) (Num a)) =>
-- --   Chars
-- --   -> Optional a
-- -- readHex =
-- --   mapOptional fst . readHexs

-- -- readFloats ::
-- --   (RealFrac a) =>
-- --   Chars
-- --   -> Optional (Tuple a Chars)
-- -- readFloats s =
-- --   case N.readSigned N.readFloat (hlist s) of
-- --     [] -> Empty
-- --     ((Tuple a q):_) -> Full (Tuple a (listh q))

-- -- readFloat ::
-- --   (RealFrac a) =>
-- --   Chars
-- --   -> Optional a
-- -- readFloat =
-- --   mapOptional fst . readFloats

-- -- instance IsString (List Char) where
-- --   fromString =
-- --     listh

-- type Chars =
--   List Char

-- type FilePath =
--   List Char

-- strconcat ::
--   Array Chars
--   -> String
-- strconcat =
--   concatMap hlist

-- stringconcat :: Array String -> String
-- stringconcat = fromCharArray

-- show' :: forall a. Show a => a -> List Char
-- show' a = listh (toCharArray (show a))

-- instance applicativeList :: Applicative List where
--   -- (<*>) =
--   --   M.ap
--   pure a = (a :. Nil)

-- instance monadList :: Bind List where
--   bind = flip flatMap
--   --return = (:. Nil)