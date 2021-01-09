module Course.Functor where

import Course.Optional
import Course.List(List(..), (:.))
import Course.ExactlyOne (ExactlyOne(..))

import Effect (Effect)
import Prelude (Unit, unit, (<<<))
import Prelude (map) as P

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  map :: forall a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance exactlyOneFunctor :: Functor ExactlyOne where
  map :: forall a b. (a -> b) -> ExactlyOne a -> ExactlyOne b
  map f (ExactlyOne a) = ExactlyOne (f a)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance listFunctor :: Functor List where
  map :: forall a b. (a -> b) -> List a -> List b
  map _ Nil = Nil
  map f (a :. as) = f a :. map f as

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance optionalFunctor :: Functor Optional where
  map :: forall a b. (a -> b) -> Optional a -> Optional b
  map f Empty = Empty
  map f (Full a) = Full (f a)

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance functionFunctor :: Functor ((->) t) where
  map :: forall a b. (a -> b) -> ((->) t a) -> ((->) t b)
  map = (<<<)

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
voidRight :: forall f a b. Functor f => a -> f b -> f a
voidRight a = map (\_ -> a)

infixl 4 voidRight as <$

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void :: forall f a. Functor f => f a -> f Unit
void = voidRight unit

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance effectFunctor :: Functor Effect where
  map = P.map