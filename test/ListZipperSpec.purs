module Course.ListZipperSpec where


import Course.Core

import Course.Applicative (pure, (<*>))
import Course.Comonad (copure)
import Course.Extend ((<<=))
import Course.Functor ((<$>))
import Course.List (List(..), all, isEmpty, take, (:.))
import Course.ListZipper (ListZipper, MaybeListZipper(..), deletePullLeft, deletePullRight, dropLefts, dropRights, end, findLeft, findRight, fromList, hasLeft, hasRight, index, insertPushLeft, insertPushRight, lefts, moveLeft, moveLeftLoop, moveLeftN, moveLeftN', moveRight, moveRightLoop, moveRightN, moveRightN', nth, rights, setFocus, start, swapLeft, swapRight, toList, toListZ, toOptional, withFocus, zipper, (-<<))
import Course.Optional (Optional(Empty, Full))
import Course.Traversable (traverse)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Monad, Unit, const, discard, identity, ($), (*), (+), (<<<), (==), negate)
import Prelude as P
import Test.QuickCheck (class Testable, quickCheck, (===))
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- prop :: forall m prop. MonadEffect m => Testable prop => String -> prop -> m Unit
prop :: forall t m2 m. Monad m => MonadEffect m2 => Testable t => String -> t -> SpecT m2 Unit m Unit
prop msg p = it msg (liftEffect $ quickCheck p)

spec :: Spec Unit
spec = do
  describe "Functor" $ do
    it "ListZipper (<$>)" $
      (((+)1) <$> zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [4,3,2] 5 [6,7,8]

  describe "Functor Maybe" $ do
    it "MaybeListZipper (<$>)" $
      (((+)1) <$> IsZ (zipper [3,2,1] 4 [5,6,7])) `shouldEqual` IsZ (zipper [4,3,2] 5 [6,7,8])

  describe "toList" $ do
    it "Optional empty list" $
      (toList <$> Empty) `shouldEqual` (Empty :: Optional (List Int))
    it "empty left" $
      toList (zipper [] 1 [2,3,4]) `shouldEqual` (1:.2:.3:.4:.Nil)
    it "lefts and rights" $
      toList (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` (1:.2:.3:.4:.5:.6:.7:.Nil)

  describe "fromList" $ do
    it "non-empty" $ fromList (1 :. 2 :. 3 :. Nil) `shouldEqual` IsZ (zipper [] 1 [2,3])
    it "empty" $ fromList Nil `shouldEqual` (IsNotZ :: MaybeListZipper Int)
    prop "round trip" $
      (\(xs :: List Int) -> toListZ (fromList xs) === xs)

  describe "toOptional" $ do
    prop "empty" $
      \(xs :: List Int) ->
        isEmpty xs === (toOptional (fromList xs) == Empty)

  describe "withFocus" $ do
    it "empty left" $
      withFocus ((+)1) (zipper [] 0 [1]) `shouldEqual` zipper [] 1 [1]
    it "left and right" $
      withFocus ((+)1) (zipper [1,0] 2 [3,4]) `shouldEqual` zipper [1,0] 3 [3,4]

  describe "setFocus" $ do
    it "empty left" $
      setFocus 1 (zipper [] 0 [1]) `shouldEqual` zipper [] 1 [1]
    it "left and right" $
      setFocus 1 (zipper [1,0] 2 [3,4]) `shouldEqual` zipper [1,0] 1 [3,4]

  describe "hasLeft" $ do
    it "left and right" $ hasLeft (zipper [1,0] 2 [3,4]) `shouldEqual` true
    it "empty left" $ hasLeft (zipper [] 0 [1,2]) `shouldEqual` false

  describe "hasRight" $ do
    it "left and right" $ hasRight (zipper [1,0] 2 [3,4]) `shouldEqual` true
    it "empty right" $ hasRight (zipper [1,0] 2 []) `shouldEqual` false

  describe "findLeft" $ do
    prop "missing element returns IsNotZ" $
      (\((xs :: List Int) /\ p) -> findLeft (const p) -<< fromList xs === IsNotZ)
    it "found in left" $
      findLeft ((==) 1) (zipper [2,1] 3 [4,5]) `shouldEqual` IsZ (zipper [] 1 [2,3,4,5])
    it "not found" $
      findLeft ((==) 6) (zipper [2,1] 3 [4,5]) `shouldEqual` IsNotZ
    it "one match in left" $
      findLeft ((==) 1) (zipper [2,1] 1 [4,5]) `shouldEqual` IsZ (zipper [] 1 [2,1,4,5])
    it "multiple matches in left" $
      findLeft ((==) 1) (zipper [1,2,1] 3 [4,5]) `shouldEqual` IsZ (zipper [2,1] 1 [3,4,5])
    it "elements shifted to right correctly" $
      findLeft ((==) 1) (zipper [3,4,1,5] 9 [2,7]) `shouldEqual` IsZ (zipper [5] 1 [4,3,9,2,7])

  describe "findRight" $ do
    prop "missing element returns IsNotZ" $
      (\(xs :: List Int) -> findRight (const false) -<< fromList xs === IsNotZ)
    it "found in right" $
      findRight ((==) 5) (zipper [2,1] 3 [4,5]) `shouldEqual` IsZ (zipper [4,3,2,1] 5 [])
    it "not found" $
      findRight ((==) 6) (zipper [2,1] 3 [4,5]) `shouldEqual` IsNotZ
    it "one match in right" $
      findRight ((==) 1) (zipper [2,3] 1 [4,5,1]) `shouldEqual` IsZ (zipper [5,4,1,2,3] 1 [])
    it "multiple matches in right" $
      findRight ((==) 1) (zipper [2,3] 1 [1,4,5,1]) `shouldEqual` IsZ (zipper [1,2,3] 1 [4,5,1])

  describe "moveLeftLoop" $ do
    it "with left" $
      moveLeftLoop (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [2,1] 3 [4,5,6,7]
    it "empty left" $
      moveLeftLoop (zipper [] 1 [2,3,4]) `shouldEqual` zipper [3,2,1] 4 []

  describe "moveRightLoop" $ do
    it "with right" $
      moveRightLoop (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [4,3,2,1] 5 [6,7]
    it "empty right" $
      moveRightLoop (zipper [3,2,1] 4 []) `shouldEqual` zipper [] 1 [2,3,4]

  describe "moveLeft" $ do
    it "with left" $
      moveLeft (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [2,1] 3 [4,5,6,7])
    it "empty left" $
      moveLeft (zipper [] 1 [2,3,4]) `shouldEqual` IsNotZ

  describe "moveRight" $ do
    it "with right" $
      moveRight (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [4,3,2,1] 5 [6,7])
    it "empty right" $
      moveRight (zipper [3,2,1] 4 []) `shouldEqual` IsNotZ

  describe "swapLeft" $ do
    it "with left" $
      swapLeft (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [4,2,1] 3 [5,6,7])
    it "empty left" $
      swapLeft (zipper [] 1 [2,3,4]) `shouldEqual` IsNotZ

  describe "swapRight" $ do
    it "with right" $
      swapRight (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [3,2,1] 5 [4,6,7])
    it "empty right" $
      swapRight (zipper [3,2,1] 4 []) `shouldEqual` IsNotZ

  describe "dropLeft" $ do
    it "with left" $
      dropLefts (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [] 4 [5,6,7]
    it "empty left" $
      dropLefts (zipper [] 1 [2,3,4]) `shouldEqual` zipper [] 1 [2,3,4]
    prop "dropLefts empties left of zipper"
      (\l x r -> dropLefts (zipper l x r) === (zipper [] x r :: ListZipper Int))

  describe "dropRights" $ do
    it "with right" $
      dropRights (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [3,2,1] 4 []
    it "empty right" $
      dropRights (zipper [3,2,1] 4 []) `shouldEqual` zipper [3,2,1] 4 []
    prop "dropRights empties right of zipper"
      (\l x r -> dropRights (zipper l x r) === (zipper l x [] :: ListZipper Int))

  describe "moveLeftN" $ do
    it "positive moves" $
      moveLeftN 2 (zipper [2,1,0] 3 [4,5,6]) `shouldEqual` IsZ (zipper [0] 1 [2,3,4,5,6])
    it "negative moves" $
      (moveLeftN (-1) (zipper [2,1,0] 3 [4,5,6])) `shouldEqual` IsZ (zipper [3,2,1,0] 4 [5,6])

  describe "moveRightN" $ do
    it "positive moves" $
      moveRightN 1 (zipper [2,1,0] 3 [4,5,6]) `shouldEqual` IsZ (zipper [3,2,1,0] 4 [5,6])
    it "negative moves" $
      moveRightN (-1) (zipper [2,1,0] 3 [4,5,6]) `shouldEqual` IsZ (zipper [1,0] 2 [3,4,5,6])

  describe "moveLeftN'" $ do
    it "positive - out of bounds both sides" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Left 3
    it "positive in range" $
      moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Right (zipper [2,1] 3 [4,5,6,7])
    prop "moving zero is `Right . id`"
      (\l x r -> let lz = zipper l x r :: ListZipper Int
                  in moveLeftN' 0 lz === (Right <<< identity $ lz))
    it "negative in range" $
      moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Right (zipper [5,4,3,2,1] 6 [7])
    it "negative out of bounds" $
      moveLeftN' (-4 ) (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Left 3
    it "positive - out of bounds on left only" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9]) `shouldEqual` Left 3
    it "negative - out of bounds on right only" $
      moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9]) `shouldEqual` Left 3

  describe "moveRightN'" $ do
    it "positive - out of bounds both sides" $
      moveRightN' 4 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Left 3
    it "positive in range" $
      moveRightN' 1 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Right (zipper [4,3,2,1] 5 [6,7])
    prop "moving zero is `Right <<< id`"
      (\l x r -> let lz = (zipper l x r :: ListZipper Int) in moveRightN' 0 lz === (Right <<< identity $ lz))
    it "negative in range" $
      moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Right (zipper [1] 2 [3,4,5,6,7])
    it "negative - out of bounds both sides" $
      moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` Left 3

  describe "nth" $ do
    it "have 1"    $ nth 1 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [1] 2 [3,4,5,6,7])
    it "have 5"    $ nth 5 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [5,4,3,2,1] 6 [7])
    it "missing 8" $ nth 8 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsNotZ

  describe "index" $ do
    it "index works" $ index (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` 3
    prop "Always returns the index on a valid zipper" $
      \((z :: ListZipper Int) /\ i) ->
        optional true (\z' -> index z' == i) (toOptional (nth i z))

  describe "end" $ do
    it "end" $ end (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [6,5,4,3,2,1] 7 []
    prop "end never changes the zipper's contents" $
      (\(z :: ListZipper Int) -> toList z === toList (end z))
    prop "never have rights after calling end" $
      (\(z :: ListZipper Int) -> rights (end z) === Nil)

  describe "start" $ do
    it "start" $ start (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [] 1 [2,3,4,5,6,7]
    prop "start never changes the zipper's contents" $
      (\(z :: ListZipper Int) -> toList z === toList (start z))
    prop "never have lefts after calling start" $
      (\(z :: ListZipper Int) -> lefts (start z) === Nil)

  describe "deletePullLeft" $ do
    it "non-empty lefts" $ deletePullLeft (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [2,1] 3 [5,6,7])
    it "empty lefts" $ deletePullLeft (zipper [] 1 [2,3,4]) `shouldEqual` IsNotZ

  describe "deletePullRight" $ do
    it "non-empty rights" $ deletePullRight (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` IsZ (zipper [3,2,1] 5 [6,7])
    it "empty rights" $ deletePullRight (zipper [3,2,1] 4 []) `shouldEqual` IsNotZ

  describe "insertPushLeft" $ do
    it "non-empty lefts" $
      insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [4,3,2,1] 15 [5,6,7]
    it "empty lefts" $
      insertPushLeft 15 (zipper [] 1 [2,3,4]) `shouldEqual` zipper [1] 15 [2,3,4]
    prop "deletePullLeft . insertPushLeft === id" $
      \(z /\ i) ->
        optional
          false
          ((==) z)
          (toOptional (deletePullLeft (insertPushLeft (toNumber i) z)))

  describe "insertPushRight" $ do
    it "non-empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [3,2,1] 15 [4,5,6,7]
    it "empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 []) `shouldEqual` zipper [3,2,1] 15 [4]
    prop "deletePullRight . insertPushRight === id" $
      \(z/\i) ->
        optional
          false
          ((==) z)
          (toOptional (deletePullRight (insertPushRight (toNumber i) z)))

  describe "Applicative" $ do
    prop "pure produces infinite lefts"
      (\a n -> (all <<< (==) <*> take (n :: Int) <<< lefts <<< pure) (a :: Int))
    prop "pure produces infinite rights"
      (\a n -> (all <<< (==) <*> take (n :: Int) <<< rights <<< pure) (a :: Int))
    it "<*> applies functions to corresponding elements in zipper" $
      (zipper [((+)2), ((+)10)] ((*)2) [((*)3), ((*)4), ((+)5)] <*> zipper [3,2,1] 4 [5,6,7]) `shouldEqual` zipper [5,12] 8 [15,24,12]

  let
    is (IsZ z) = z
    is _       = error "MaybeListZipper's Applicative instances is busted"
    notZ       = IsNotZ :: MaybeListZipper Int

  describe "Applicative (MaybeListZipper)" $ do
    prop "pure produces infinite lefts"
      (\a n -> (all <<< (==) <*> take (n :: Int) <<< lefts <<< is <<< pure) (a :: Int))
    prop "pure produces infinite rights"
      (\a n -> (all <<< (==) <*> take (n :: Int) <<< rights <<< is <<< pure) (a :: Int))
    it "IsZ <*> IsZ" $
      let z = IsZ (zipper [((+)2), ((+)10)] ((*)2) [((*)3), ((*)4), ((+)5)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
       in z `shouldEqual` IsZ (zipper [5,12] 8 [15,24,12])
    prop "IsNotZ <*> IsZ" $
      let fs = (IsNotZ :: MaybeListZipper (Int -> Int))
       in (\z -> (fs <*> IsZ z) === IsNotZ)
    -- prop "IsZ <*> IsNotZ"
    --   (\(Fun _ f) -> (IsZ (pure f) <*> notZ) === notZ)
    it "IsNotZ <*> IsNotZ" $
      (IsNotZ <*> notZ) `shouldEqual` notZ

  describe "Extend" $ do
    it "zipper o' zippers" $ do
      let
        z = zipper [2,1] 3 [4,5]
        l = [zipper [1] 2 [3,4,5], zipper [] 1 [2,3,4,5]]
        r = [zipper [3,2,1] 4 [5], zipper [4,3,2,1] 5 []]
      (identity <<= z) `shouldEqual` zipper l z r

  describe "Extend (MaybeListZipper)" $ do
    it "IsNotZ" $
      (identity <<= IsNotZ) `shouldEqual` (IsNotZ :: MaybeListZipper (MaybeListZipper Int))
    it "IsZ" $ do
      let
        z = IsZ (zipper [2,1] 3 [4,5])
        l = IsZ P.<$> [zipper [1] 2 [3,4,5], zipper [] 1 [2,3,4,5]]
        r = IsZ P.<$> [zipper [3,2,1] 4 [5], zipper [4,3,2,1] 5 []]
      (identity <<= z) `shouldEqual` IsZ (zipper l z r)

  describe "Comonad" $ do
    it "copure" $ copure (zipper [2,1] 3 [4,5]) `shouldEqual` 3

  describe "Traversable" $ do
    prop "All Full" $
      (\(z :: ListZipper Int) -> traverse identity (Full <$> z) === Full z)
    it "One Empty" $
      traverse identity (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
        `shouldEqual`
          Empty

  describe "Traversable (MaybeListZipper)" $ do
    it "IsNotZ" $
      traverse identity IsNotZ `shouldEqual` (Full IsNotZ :: Optional (MaybeListZipper Int))
    prop "IsZ Full" $
      (\(z :: ListZipper Int) -> traverse identity (Full <$> IsZ z) === Full (IsZ z))

optional :: forall a b. b -> (a -> b) -> Optional a -> b
optional e _ Empty    = e
optional _ f (Full a) = f a