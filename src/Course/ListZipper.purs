module Course.ListZipper where

import Course.Applicative
import Course.Comonad
import Course.Core
import Course.Extend
import Course.Functor
import Course.List
import Course.Optional
import Course.Traversable

import Data.Array (foldl)
import Data.Either (Either(..))
import Prelude (class Eq, class Show, append, flip, show, (<<<), bind)
import Prelude as P
import Test.QuickCheck (class Arbitrary, arbitrary)
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
--  deriving Eq
derive instance eqListZipper :: Eq a => Eq (ListZipper a)

lefts :: forall a.
  ListZipper a
  -> List a
lefts (ListZipper l _ _) =
  l

rights :: forall a.
  ListZipper a
  -> List a
rights (ListZipper _ _ r) =
  r

-- RMD added code for quickcheck
instance arbitraryListZipper :: Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary = zipper P.<$> arbitrary P.<*> arbitrary P.<*> arbitrary
-- RMD added code for quickcheck

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
--  deriving Eq
derive instance eqMaybeListZipper :: Eq a => Eq (MaybeListZipper a)

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance functorListZipper :: Functor ListZipper where
  map = \_ _ -> ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#instance ListZipper") Nil

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance functorMaybeListZipper :: Functor MaybeListZipper where
  map = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#instance MaybeListZipper") Nil)

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList :: forall a.
  ListZipper a
  -> List a
toList = \_ ->  (unsafeCoerce "todo: Course.ListZipper#toList") :. Nil

-- | Convert the given (maybe) zipper back to a list.
toListZ :: forall a.
  MaybeListZipper a
  -> List a
toListZ IsNotZ =
  Nil
toListZ (IsZ z) =
  toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList :: forall a.
  List a
  -> MaybeListZipper a
fromList = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#fromList") Nil)

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional :: forall a.
  MaybeListZipper a
  -> Optional (ListZipper a)
toOptional = \_ -> Full (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#toOptional") Nil)

zipper :: forall a.
  Array a
  -> a
  -> Array a
  -> ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional :: forall a.
  Optional (ListZipper a)
  -> MaybeListZipper a
fromOptional Empty =
  IsNotZ
fromOptional (Full z) =
  IsZ z

asZipper :: forall a.
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asZipper f =
  asMaybeZipper (IsZ <<< f)

infixl 4 asZipper as >$>

asMaybeZipper :: forall a.
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asMaybeZipper _ IsNotZ =
  IsNotZ
asMaybeZipper f (IsZ z) =
  f z

infixl 4 asMaybeZipper as -<<

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus :: forall a.
  (a -> a)
  -> ListZipper a
  -> ListZipper a
withFocus = \_ _ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#withFocus") Nil)

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus :: forall a.
  a
  -> ListZipper a
  -> ListZipper a
setFocus = \_ _ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#setFocus") Nil)

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
flipSetFocus :: forall a.
  ListZipper a
  -> a
  -> ListZipper a
flipSetFocus =
  flip setFocus

infixl 4 asZipper as .=

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft :: forall a.
  ListZipper a
  -> Boolean
hasLeft = \_ -> (unsafeCoerce "todo: Course.ListZipper#hasLeft")

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight :: forall a.
  ListZipper a
  -> Boolean
hasRight = \_ -> (unsafeCoerce "todo: Course.ListZipper#hasRight")

-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == IsNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]
findLeft :: forall a.
  (a -> Boolean)
  -> ListZipper a
  -> MaybeListZipper a
findLeft = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#findLeft") Nil)
    
-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == IsNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight :: forall a.
  (a -> Boolean)
  -> ListZipper a
  -> MaybeListZipper a
findRight = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#findRight") Nil)

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop :: forall a.
  ListZipper a
  -> ListZipper a
moveLeftLoop = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveLeftLoop") Nil)

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop :: forall a.
  ListZipper a
  -> ListZipper a
moveRightLoop = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveRightLoop") Nil)

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft :: forall a.
  ListZipper a
  -> MaybeListZipper a
moveLeft = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveLeft") Nil)

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight :: forall a.
  ListZipper a
  -> MaybeListZipper a
moveRight = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveRight") Nil)

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft :: forall a.
  ListZipper a
  -> MaybeListZipper a
swapLeft = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#swapLeft") Nil)

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight :: forall a.
  ListZipper a
  -> MaybeListZipper a
swapRight = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#swapRight") Nil)

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts :: forall a.
  ListZipper a
  -> ListZipper a
dropLefts = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#dropLefts") Nil)

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights :: forall a.
  ListZipper a
  -> ListZipper a
dropRights = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#dropRights") Nil)

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN :: forall a.
  Int
  -> ListZipper a
  -> MaybeListZipper a
moveLeftN = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveLeftN") Nil)

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN :: forall a.
  Int
  -> ListZipper a
  -> MaybeListZipper a
moveRightN = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveRightN") Nil)

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
moveLeftN' :: forall a.
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
moveLeftN' = \_ _ -> Right (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveLeftN'") Nil)

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' :: forall a.
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
moveRightN' = \_ _ -> Right (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#moveRightN'") Nil)

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth :: forall a.
  Int
  -> ListZipper a
  -> MaybeListZipper a
nth = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#nth") Nil)

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z z' -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index :: forall a.
  ListZipper a
  -> Int
index = \_ -> (unsafeCoerce "todo: Course.ListZipper#index")

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end :: forall a.
  ListZipper a
  -> ListZipper a
end = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#end") Nil)

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start :: forall a.
  ListZipper a
  -> ListZipper a
start = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#start") Nil)

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft :: forall a.
  ListZipper a
  -> MaybeListZipper a
deletePullLeft = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#deletePullLeft") Nil)

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight :: forall a.
  ListZipper a
  -> MaybeListZipper a
deletePullRight = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#deletePullRight") Nil)

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft :: forall a.
  a
  -> ListZipper a
  -> ListZipper a
insertPushLeft = \_ _ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#insertPushLeft") Nil)

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight :: forall a.
  a
  -> ListZipper a
  -> ListZipper a
insertPushRight = \_ _ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<$>)#insertPushRight") Nil)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance applicativeListZipper :: Applicative ListZipper where
-- /Tip:/ Use @List#repeat@.
  pure = \_ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper pure#instance ListZipper") Nil)
-- /Tip:/ Use `zipWith`
  apply = \_ _ -> (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<*>)#instance ListZipper") Nil)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> IsNotZ <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsNotZ
-- ><
--
-- >>> IsNotZ <*> IsNotZ
-- ><
instance applicativeMaybeListZipper :: Applicative MaybeListZipper where
  pure = \_ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper pure#instance MaybeListZipper") Nil)
  apply = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (<*>)#instance MaybeListZipper") Nil)

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance extendListZipper :: Extend ListZipper where
  extend = \_ _ -> (ListZipper Nil (unsafeCoerce (ListZipper Nil "todo: Course.ListZipper (<<=)#instance ListZipper" Nil)) Nil)

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= IsNotZ
-- ><
--
-- >>> id <<= (IsZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance extendMaybeListZipper :: Extend MaybeListZipper where
  extend = \_ _ -> IsZ (ListZipper Nil (unsafeCoerce (IsZ (ListZipper Nil "todo: Course.ListZipper (<<=)#instance MaybeListZipper" Nil))) Nil)

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance comonadListZipper :: Comonad ListZipper where
  copure = \_ -> unsafeCoerce "todo: Course.ListZipper copure#instance ListZipper"

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
instance traversableListZipper :: Traversable ListZipper where
  traverse = \_ _ -> (unsafeCoerce (Full (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (traverse)#instance ListZipper") Nil)))
    --error "todo: Course.ListZipper traverse#instance ListZipper"

-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id IsNotZ
-- ><
--
-- >>> traverse id (IsZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance maybeListZipper :: Traversable MaybeListZipper where
  traverse = \_ _ -> (unsafeCoerce (Full (IsZ (ListZipper Nil (unsafeCoerce "todo: Course.ListZipper (traverse)#instance MaybeListZipper") Nil))))

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance showListZipper :: Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    foldl append "" [show l, " >", show x, "< ", show r]

instance showMaybeListZipper :: Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"