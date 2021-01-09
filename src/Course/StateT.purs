module Course.StateT where

import Course.Applicative (class Applicative, pure)
import Course.Core (error)
import Course.ExactlyOne (ExactlyOne(..))
import Course.Functor (class Functor, map)
import Course.List (List(..))
import Course.Monad (class Monad, bind)
import Course.Optional (Optional(..))

import Data.Identity (Identity(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (class Eq, class Show, show, unit, ($))
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
  map _ _ = StateT \_ -> unsafeCoerce Nil --error "todo: Course.StateT (<$>)#instance (StateT s f)"
  -- map atob (StateT stof) = StateT \s -> map (uncurry foo) (stof s) where
    -- foo a s' = atob a /\ s'
    

infixl 4 map as <$>


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
  pure :: forall s f a. a -> StateT s f a
  pure _ = StateT \_ -> unsafeCoerce Nil --error "todo: Course.StateT pure#instance (StateT s f)"
  apply :: forall s f a b. Monad f => StateT s f (a -> b) -> StateT s f a -> StateT s f b
  -- apply = error "todo: Course.StateT (<*>)#instance (StateT s f)"
  apply _ st = unsafeCoerce st -- error "todo: Course.StateT (<*>)#instance (StateT s f)"

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance monadStateT :: Monad f => Monad (StateT s f) where
  bind :: forall s f a b. (a -> StateT s f b) -> StateT s f a -> StateT s f b
  bind _ st = unsafeCoerce st --error "todo: Course.StateT (=<<)#instance (StateT s f)"

infixl 4 bind as =<<

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' :: forall s a. (s -> Tuple a s) -> State' s a
state' _ = StateT \_ -> ExactlyOne $ unsafeCoerce (unit /\ 99) --error "todo: Course.StateT#state'"

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: forall s a. State' s a -> s -> Tuple a s
runState' _ _ = unsafeCoerce (unit /\ unit) --error "todo: Course.StateT#runState'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: forall s f a. Functor f => StateT s f a -> s -> f s
execT _ s = error "todo: Course.StateT#execT"

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: forall s a.
  State' s a
  -> s
  -> s
exec' =
  error "todo: Course.StateT#exec'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: forall s f a.
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT =
  error "todo: Course.StateT#evalT"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' :: forall s a.
  State' s a
  -> s
  -> a
eval' =
  error "todo: Course.StateT#eval'"

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: forall s f. Applicative f => StateT s f s
getT = unsafeCoerce Identity \_ -> Nil --error "todo: Course.StateT#getT"

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: forall s f. Applicative f => s -> StateT s f P.Unit
putT _ = unsafeCoerce Identity \_ -> Nil --error "todo: Course.StateT#putT"

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: forall a.
  P.Ord a => P.Semiring a =>
  List a
  -> List a
distinct' =
  error "todo: Course.StateT#distinct'"

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
distinctF :: forall a. P.Ord a => P.Semiring a => List a -> Optional (List a)
distinctF _ = Empty --error "todo: Course.StateT#distinctF"

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a = OptionalT (f (Optional a))

runOptionalT :: forall a f. Functor f => OptionalT f a -> f (Optional a)
runOptionalT (OptionalT f) = f --error "todo"

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance mapOptionalT :: Functor f => Functor (OptionalT f) where
  map _ ft = unsafeCoerce ft --error "todo: Course.StateT (<$>)#instance (OptionalT f)"

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
  pure = error "todo: Course.StateT pure#instance (OptionalT f)"
  apply _ ot2 = unsafeCoerce ot2 --error "todo: Course.StateT (<*>)#instance (OptionalT f)"

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- -- [Full 2,Full 3,Empty]
instance monadOptionalT :: Monad f => Monad (OptionalT f) where
  bind _ ot = unsafeCoerce ot --error "todo: Course.StateT (=<<)#instance (OptionalT f)"

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
  map _ l = unsafeCoerce l --error "todo: Course.StateT (<$>)#instance (Logger l)"

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance applicativeLogger :: Applicative (Logger l) where
  pure _ = unsafeCoerce (Logger Nil "error - todo: Course.StateT pure#instance (Logger l)")
  apply _ l = unsafeCoerce l --error "todo: Course.StateT (<*>)#instance (Logger l)"

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance monadLogger :: Monad (Logger l) where
  bind _ l = unsafeCoerce l --error "todo: Course.StateT (=<<)#instance (Logger l)"

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: forall l a. l -> a -> Logger l a
log1 _ _ = unsafeCoerce (Logger Nil "todo: Course.StateT#log1")

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
distinctG :: forall a. P.Semiring a => P.Show a =>
  List a -> Logger (List Char) (Optional (List a))
distinctG _ = Logger Nil Empty -- error "todo: Course.StateT#distinctG"

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
