module Course.Comonad where

import Course.Core
import Course.ExactlyOne
import Course.Extend

import Course.Functor (map)

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend f <= Comonad f where
  copure :: forall a. f a -> a

-- | Implement the @Comonad@ instance for @ExactlyOne@.
--
-- >>> copure (ExactlyOne 7)
-- 7
instance exactlyOneComonad :: Comonad ExactlyOne where
  copure :: forall a. ExactlyOne a -> a
  copure (ExactlyOne a) = a

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$$> ExactlyOne 7
-- ExactlyOne 17
mapComonad :: forall f a b. Comonad f => (a -> b) -> f a -> f b
mapComonad = map

infixl 4 mapComonad as <$$>
