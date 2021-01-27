module Course.StateT where

import Control.Category (identity)
import Course.Applicative (class Applicative, apply, filtering, lift2, pure, (<*>))
import Course.Core (error)
import Course.ExactlyOne (ExactlyOne(..), runExactlyOne)
import Course.Functor (class Functor, map, (<$>))
import Course.List (List(..), listh, (:.))
import Course.Monad (class Monad, bind, (=<<), (>>=))
import Course.Optional (Optional(..), optional)
import Data.Identity (Identity(..))
import Data.Ord (class Ord)
import Data.Set as S
import Data.String.Utils (toCharArray)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (over1, (/\))
import Prelude (class Eq, class Show, Unit, mod, not, show, unit, ($), (<<<), (<>), (==), (>))
import Prelude as P
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
--newtype StateT s m a = StateT (s -> m (Tuple a s))
newtype StateT s f a = StateT (s -> f (Tuple a s))

runStateT :: forall s f a. StateT s f a -> s -> f (Tuple a s)
runStateT (StateT stof) s = stof s


-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance functorStateT :: Functor f => Functor (StateT s f) where
  map :: forall s a f b. Functor f => (a -> b) -> StateT s f a -> StateT s f b
  map atob (StateT f) = StateT \s -> over1 atob <$> f s

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance pureStateT :: Monad f => Applicative (StateT s f) where
  --TODO: add type constraint to starter for pure
  pure :: forall s f a. Monad f => a -> StateT s f a
  pure a = StateT $ \s -> pure $ a /\ s
  apply :: forall s f a b. Monad f => StateT s f (a -> b) -> StateT s f a -> StateT s f b
  apply (StateT smf) (StateT sma) = StateT \s -> smf s >>= \(f/\s') -> over1 f <$> sma s'

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance monadStateT :: Monad f => Monad (StateT s f) where
  bind :: forall s f a b. Monad f => (a -> StateT s f b) -> StateT s f a -> StateT s f b
  bind f (StateT sma) = StateT \s -> sma s >>= \(a/\s') -> runStateT (f a) s'

--TODO: remove from starter
--infixl 4 bind as =<<

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' :: forall s a. (s -> Tuple a s) -> State' s a
state' f = StateT \s -> pure $ f s

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: forall s a. State' s a -> s -> Tuple a s
runState' st' = runExactlyOne <<< runStateT st'

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: forall s f a. Functor f => StateT s f a -> s -> f s
execT (StateT sma) s = snd <$> sma s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: forall s a. State' s a -> s -> s
exec' st' = snd <<< runState' st'

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: forall s f a. Functor f => StateT s f a -> s -> f a
evalT (StateT sma) s = fst <$> sma s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' :: forall s a. State' s a -> s -> a
eval' st' = fst <<< runState' st'

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: forall s f. Applicative f => StateT s f s
getT = StateT \s -> pure $ s/\s

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: forall s f. Applicative f => s -> StateT s f P.Unit
putT s = StateT \_ -> pure $ unit/\s

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: forall a. P.Ord a => P.Semiring a => List a -> List a
distinct' as = eval' (filtering (\a -> state' \s -> (not $ S.member a s) /\ S.insert a s) as) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
-- NOTE: had to change type signature RMD, no type class Num in purescript
distinctF :: List Int -> Optional (List Int)
distinctF as = evalT (filtering (\a -> StateT \s -> if a > 100 then Empty else Full ((not $ S.member a s) /\ S.insert a s)) as) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a = OptionalT (f (Optional a))

runOptionalT :: forall a f. Functor f => OptionalT f a -> f (Optional a)
runOptionalT (OptionalT fa) = fa

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance mapOptionalT :: Functor f => Functor (OptionalT f) where
  map f (OptionalT fa) = OptionalT $ map f <$> fa

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil) 
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance pureOptionalT :: Monad f => Applicative (OptionalT f) where
  pure = OptionalT <<< pure <<< Full
  apply :: forall f a b. Monad f => OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  apply (OptionalT fOptAtoB) (OptionalT fOptA) = OptionalT $ fOptAtoB >>= onFull \atob -> map (map atob) fOptA

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- -- [Full 2,Full 3,Empty]
instance monadOptionalT :: Monad f => Monad (OptionalT f) where
  bind :: forall f a b. Monad f => (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  bind f (OptionalT fOptA) = OptionalT $ fOptA >>= onFull (runOptionalT <<< f)

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a = Logger (List l) a
derive instance eqLogger :: (Eq ls, Eq a) => Eq (Logger ls a)
instance showLogger :: (Show ls, Show a) => Show (Logger ls a) where
  show (Logger ls a) = show ls P.<> show a

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance functorLogger :: Functor (Logger l) where
  map :: forall a b. (a -> b) -> Logger l a -> Logger l b
  map f (Logger ls a) = Logger ls $ f a

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance applicativeLogger :: Applicative (Logger l) where
  pure :: forall a l. a -> Logger l a
  pure a = Logger Nil a
  apply :: forall l a b. Logger l (a -> b) -> Logger l a -> Logger l b
  apply (Logger ls' f) (Logger ls a) = Logger (ls' <> ls) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance monadLogger :: Monad (Logger l) where
  bind :: forall l a b. (a -> Logger l b) -> Logger l a -> Logger l b
  bind f (Logger ls a) = (Logger ls identity) <*> f a

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: forall l a. l -> a -> Logger l a
log1 l a = Logger (l :. Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: List Int -> Logger String (Optional (List Int))
distinctG ns = runOptionalT $ evalT (filtering (\n -> StateT \s -> OptionalT $ testN n s) ns) S.empty where
  testN :: Int -> S.Set Int -> Logger String (Optional (Tuple Boolean (S.Set Int)))
  testN n s =
    if n > 100 then log1 ("aborting > 100: " <> show n) Empty
    else
      if n `mod` 2 == 1 then pure (Full $ (not $ S.member n s) /\ (S.insert n s))
      else log1 ("even number: " <> show n) (Full $ (not $ S.member n s) /\ (S.insert n s))

onFull :: forall f t a.
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
