module Course.Applicative where

import Course.ExactlyOne (ExactlyOne(..))
import Course.Functor (class Functor, map, (<$>))
import Course.List (List(..), foldRight, replicate, (:.))
import Course.Optional (Optional(..), mapOptional)
import Effect (Effect)
import Prelude (($), (<>))
import Prelude (identity, map, (>>=), pure) as P

-- | All instances of the `Applicative` type-class must satisfy three laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of associative composition
--   `∀a b c. ((.) <$> a <*> b <*> c) ≅ (a <*> (b <*> c))`
--
-- * The law of identity
--   `∀x. pure id <*> x ≅ x`
--
-- * The law of homomorphism
--   `∀f x. pure f <*> pure x ≅ pure (f x)`
--
-- * The law of composition
--   `∀u v w. pure (.) <*> u <*> v <*> w ≅ u <*> (v <*> w)`
class
  Functor f <= Applicative f where
  pure :: forall a. a -> f a
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>

-- | Insert into ExactlyOne.
--
-- prop> \x -> pure x == ExactlyOne x
--
-- >>> ExactlyOne (+10) <*> ExactlyOne 8
-- ExactlyOne 18
instance exactlyOneApplicative :: Applicative ExactlyOne where
  pure :: forall a. a -> ExactlyOne a
  pure = ExactlyOne
  apply :: forall a b. ExactlyOne (a -> b) -> ExactlyOne a -> ExactlyOne b
  apply (ExactlyOne f) (ExactlyOne a) = ExactlyOne (f a)

-- | Insert into a List.
--
-- prop> \x -> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance applicativeList :: Applicative List where
  pure :: forall a. a -> List a
  pure a = a :. Nil
  apply :: forall a b. List (a -> b) -> List a -> List b
  apply Nil _ = Nil
  apply (f :. fs) as = P.map f as <> apply fs as

-- | Insert into an Optional.
--
-- prop> \x -> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance applicativeOptional :: Applicative Optional where
  pure :: forall a. a -> Optional a
  pure = Full
  apply :: forall a b. Optional (a -> b) -> Optional a -> Optional b
  apply Empty _ = Empty
  apply (Full f) optA = mapOptional f optA

-- | Insert into a constant function.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- prop> \x y -> pure x y == x
instance applicativeFunction :: Applicative ((->) t) where
  -- a function's type variable is it's parameter?
  pure :: forall a. a -> ((->) t a)
  pure a = \t -> a
  apply :: forall a b. ((->) t (a -> b)) -> ((->) t a) -> ((->) t b)
  apply f1 f2 = \t -> f1 t (f2 t)

-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
-- ExactlyOne 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
lift2 ::
  forall f a b c.
  Applicative f =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
lift2 f fa fb = f <$> fa <*> fb -- <*> = apply :: f (a -> b) -> f a -> f b 
--lift2 f fa fb = pure f <*> fa <*> fb -- <*> = apply :: f (a -> b) -> f a -> f b 

-- | Apply a ternary function in the environment.
-- /can be written using `lift2` and `(<*>)`./
--
-- >>> lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
-- ExactlyOne 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  forall f a b c d.
  Applicative f =>
  (a -> b -> c -> d) ->
  f a ->
  f b ->
  f c ->
  f d
-- lift3 f fa fb fc = unsafeCoerce fc -- error "todo: Course.Applicative#lift3"
lift3 f fa fb fc = lift2 f fa fb <*> fc  -- error "todo: Course.Applicative#lift3"

-- | Apply a quaternary function in the environment.
-- /can be written using `lift3` and `(<*>)`./
--
-- >>> lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
-- ExactlyOne 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empt-
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 :: forall f a b c d e. Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4 f fa fb fc fd = lift3 f fa fb fc <*> fd -- error "todo: Course.Applicative#lift4"
-- lift4 f fa fb fc fd = -- error "todo: Course.Applicative#lift4"

-- | Apply a nullary function in the environment.
lift0 ::
  forall f a.
  Applicative f =>
  a ->
  f a
lift0 = pure

-- | Apply a unary function in the environment.
-- /can be written using `lift0` and `(<*>)`./
--
-- >>> lift1 (+1) (ExactlyOne 2)
-- ExactlyOne 3
--
-- >>> lift1 (+1) Nil
-- []
--
-- >>> lift1 (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
lift1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
lift1 = map

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> \a b c x y z -> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> \x y -> Full x *> Full y == Full y
applySecond :: forall f a b. Applicative f => f a -> f b -> f b
applySecond fa fb = map (\_ -> P.identity) fa <*> fb

infixl 4 applySecond as *>

-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> \x y z a b c -> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> \x y -> Full x <* Full y == Full x
applyFirst :: forall f a b. Applicative f => f b -> f a -> f b
applyFirst fb fa = map (\b -> \_ -> b) fb <*> fa

infixl 4 applyFirst as <*

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence :: forall f a. Applicative f => List (f a) -> f (List a)
-- sequence fas = pure Nil
sequence = foldRight (lift2 (:.)) (pure Nil)

-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (ExactlyOne "hi")
-- ExactlyOne ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA :: forall f a. Applicative f => Int -> f a -> f (List a)
-- replicateA n fa = pure Nil
replicateA n fa = sequence $ replicate n fa

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
-- ExactlyOne [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering :: forall f a. Applicative f => (a -> f Boolean) -> List a -> f (List a)
-- filtering f as = pure Nil --error "todo: Course.Applicative#filtering"
filtering f = foldRight (\a facc -> lift2 (\bool as -> if bool then a :. as else as) (f a) facc) (pure Nil)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------
instance applicativeIO :: Applicative Effect where
  pure = P.pure
  apply f a = f P.>>= \f' -> P.map f' a

return ::
  forall f a.
  Applicative f =>
  a ->
  f a
return = pure

-- fail ::
--   Applicative f =>
--   Chars ->
--   f a
-- fail = error.hlist
-- (>>) ::
--   Applicative f =>
--   f a
--   -> f b
--   -> f b
-- (>>) =
--   (*>)
