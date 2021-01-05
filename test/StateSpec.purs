module Course.StateSpec where

import Course.Monad ((=<<), (>>=))

import Course.Applicative (pure, (<*>))
import Course.Functor ((<$>))
import Course.List (List(..), filter, flatMap, hlist, length, listh, span, (:.))
import Course.Optional (Optional(..))
import Course.State (State(..), distinct, eval, exec, findM, firstRepeat, get, isHappy, put, runState)
import Data.Array (nub)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Monad, Unit, unit, ($), discard, (+), (*), (<>), const, (==), (/=), (>))
import Test.QuickCheck (class Testable, (===), quickCheck)
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- prop :: forall m prop. MonadEffect m => Testable prop => String -> prop -> m Unit
prop :: forall t m2 m. Monad m => MonadEffect m2 => Testable t => String -> t -> SpecT m2 Unit m Unit
prop msg p = it msg (liftEffect $ quickCheck p)

spec :: Spec Unit
spec = do
  describe "State methods" $ do
    prop "exec" $
      \(f :: Int -> Tuple Int Int) s -> exec (State f) s == snd (runState (State f) s)

    prop "eval" $
      \(f :: Int -> Tuple Int Int) s -> eval (State f) s == fst (runState (State f) s)

    it "get" $ runState get 0 `shouldEqual` Tuple 0 0

    it "put" $ runState (put 1) 0 `shouldEqual` Tuple unit 1

    it "(<$>)" $
      (runState (((+)1) <$> State (\s -> Tuple 9 (s * 2))) 3) `shouldEqual` Tuple 10 6

  describe "Applicative" $ do
    it "pure" $ runState (pure 2) 0 `shouldEqual` Tuple 2 0
    it "<*>" $ runState (pure ((+)1) <*> pure 0) 0 `shouldEqual` Tuple 1 0
    it "complicated <*>" $
      let state = State (\s -> Tuple ((+)3) (s <> ["apple"])) <*> State (\s -> Tuple 7 (s <> ["banana"]))
       in runState state [] `shouldEqual` Tuple 10 ["apple","banana"]

  describe "Monad" $ do
    it "(=<<)" $
      runState ((const $ put 2) =<< put 1) 0 `shouldEqual` Tuple unit 2
    it "(>>=)" $
      let modify f = State (\s -> Tuple unit (f s))
       in runState (modify ((+)1) >>= \_ -> modify ((*)2)) 7  `shouldEqual` Tuple unit 16

  describe "findM" $ do
    it "find 'c' in 'a'..'h'" $
      let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a','b','c','d','e','f','g','h']) 0 `shouldEqual` Tuple (Full 'c') 3
    it "find 'i' in 'a'..'h'" $
      let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a','b','c','d','e','f','g','h']) 0 `shouldEqual` Tuple Empty 8

  describe "firstRepeat" $ do
    prop "finds repeats" $ \(xs :: List Int) ->
      case firstRepeat xs of
        Empty ->
          let xs' = hlist xs
           in nub xs' == xs'
        Full x -> length (filter ((==) x) xs) > 1
    prop "" $ \(xs :: List String) ->
      case firstRepeat xs of
        Empty -> true
        Full x ->
          let
            --(l, (rx :. rs)) = span (/= x) xs
            tuple1 = span ((/=) x) xs
            l = fst tuple1
            tuple2 = case snd tuple1 of
              Nil -> Tuple "" Nil
              (head :. tail) -> Tuple head tail
            rx = fst tuple2
            rs = snd tuple2
            --(l2, _) = span (/= x) rs
            l2 = fst (span ((/=) x) rs)
            l3 = hlist (l <> (rx :. Nil) <> l2)
          in
            nub l3 == l3

  describe "distinct" $ do
    prop "No repeats after distinct" $
      \(xs :: List Int) -> (firstRepeat (distinct xs)) === Empty
    prop "" $
      (\(xs :: List Int) -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs))
    it "Remove repeats - verify empty list doesn't always pass" $
      distinct (1 :. 2 :. 2 :. 1 :. 3 :. Nil) `shouldEqual` (1 :. 2 :. 3 :. Nil)
  describe "isHappy" $ do
    it "4" $ isHappy 4 `shouldEqual` false
    it "7" $ isHappy 7 `shouldEqual` true
    it "42" $ isHappy 42 `shouldEqual`  false
    it "44" $ isHappy 44 `shouldEqual`  true
