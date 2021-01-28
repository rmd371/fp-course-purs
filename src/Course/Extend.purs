module Course.Extend where

import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional

import Prelude (identity, ($), (<<<))

-- | All instances of the `Extend` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g. (f <<=) . (g <<=) ≅ (<<=) (f . (g <<=))`
class Functor f <= Extend f where
  -- Pronounced, extend.
  extend :: forall a b.
    (f a -> b)
    -> f a
    -> f b

infixr 1 extend as <<=

-- | Implement the @Extend@ instance for @ExactlyOne@.
--
-- >>> id <<= ExactlyOne 7
-- ExactlyOne (ExactlyOne 7)
instance exactlyOneExtend :: Extend ExactlyOne where
  extend :: forall a b. (ExactlyOne a -> b) -> ExactlyOne a -> ExactlyOne b
  extend = (<<<) ExactlyOne

-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]
instance listExtend :: Extend List where
  extend :: forall a b. (List a -> b) -> List a -> List b
  extend _ Nil = Nil
  extend f (a :. as) = (f $ a :. as) :. extend f as

-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty
instance optionalExtend :: Extend Optional where
  extend :: forall a b. (Optional a -> b) -> Optional a -> Optional b
  extend f oa = (\_ -> f oa) <$> oa

-- | Duplicate the functor using extension.
--
-- >>> cojoin (ExactlyOne 7)
-- ExactlyOne (ExactlyOne 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty
cojoin :: forall f a. Extend f => f a -> f (f a)
cojoin = extend identity
