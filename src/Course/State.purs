module Course.State where

import Control.Monad (join)
import Course.Applicative (class Applicative, apply, filtering, pure)
import Course.Functor (class Functor, map)
import Course.List (List(..), hlist, listh, produceN, sum, (:.))
import Course.Monad (class Monad, bind, (>>=))
import Course.Optional (Optional(..), contains)
import Data.Char.Unicode (digitToInt)
import Data.Maybe (fromMaybe)
import Data.Ord (class Ord)
import Data.Set (delete, empty, fromFoldable, insert, member) as S
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, show, unit, ($), (*), (<<<))

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a = State (s -> Tuple a s)

runState  :: forall s a. State s a -> s -> Tuple a s
runState (State f) = f

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec :: forall s a. State s a -> s -> s
exec state = snd <<< runState state

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval :: forall s a. State s a -> s -> a
eval state = fst <<< runState state

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: forall s. State s s
get = State \s -> s /\ s

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: forall s. s -> State s Unit
put s = State \_ -> unit /\ s

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance functorState :: Functor (State s) where
  map :: forall s a b. (a -> b) -> State s a -> State s b
  map mapFn (State f) = State $ uncurry go <<< f where -- uncurry is used to parse tuples
    go a = (/\) $ mapFn a

infixl 4 map as <$>

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance applicativeState :: Applicative (State s) where
  pure :: forall s a. a -> State s a
  pure a = State \s -> a /\ s
  apply :: forall s a b. State s (a -> b) -> State s a -> State s b
  apply (State ff) (State f) = State $ uncurry go <<< ff where
    go atob = uncurry go2 <<< f where
      go2 a = (/\) $ atob a 

infixl 4 apply as <*>

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance monadState :: Monad (State s) where
  bind :: forall s a b. (a -> State s b) -> State s a -> State s b
  bind bindFn (State f) = State \s -> uncurry go $ f s where
    go a = runState (bindFn a)

infixl 4 bind as =<<

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM :: forall f a. Monad f => (a -> f Boolean) -> List a -> f (Optional a)
findM _ Nil = pure Empty
findM f (a:.as) = f a >>= \bool -> if bool then pure $ Full a else findM f as

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat :: forall a. Ord a => List a -> Optional a
firstRepeat as = eval findS S.empty where
  findS = findM testS as
  testS a = State \s -> S.member a s /\ S.insert a s

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct :: forall a. Ord a => List a -> List a
distinct as = eval findS (S.fromFoldable $ hlist as) where
  findS = filtering testS as
  testS a = State \s -> S.member a s /\ S.delete a s

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy :: Int -> Boolean
isHappy n = contains 1 $ firstRepeat $ produceN squareDigitsAndSum n 1000

squareDigitsAndSum :: Int -> Int
squareDigitsAndSum n = sum $ map (square <<< fromMaybe 0 <<< digitToInt) charList where
  charList :: List Char
  charList = listh $ toCharArray $ show n

square :: Int -> Int
square = join (*)
