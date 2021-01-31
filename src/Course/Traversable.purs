module Course.Traversable where

import Course.Applicative
import Course.Compose
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional

import Control.Category (identity)
import Prelude (($))

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t <= Traversable t where
  traverse :: forall f a b.
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance listTraversable :: Traversable List where
  traverse :: forall f a b. Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f = foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance exactlyOneTraversable :: Traversable ExactlyOne where
  traverse :: forall f a b. Applicative f => (a -> f b) -> ExactlyOne a -> f (ExactlyOne b)
  traverse f (ExactlyOne a) = ExactlyOne <$> f a

instance optionalTraversable :: Traversable Optional where
  traverse :: forall f a b. Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full a) = Full <$> f a

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA :: forall t f a. Applicative f => Traversable t => t (f a) -> f (t a)
sequenceA = traverse identity

instance composeTraversable :: (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- Implement the traverse function for a Traversable instance for Compose
  traverse :: forall f' a b. Applicative f' => (a -> f' b) -> Compose f g a -> f' (Compose f g b)
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a = Product (f a) (g a)

instance functorProduct :: (Functor f, Functor g) => Functor (Product f g) where
  -- Implement the (<$>) function for a Functor instance for Product
  map :: forall a b. (a -> b) -> Product f g a -> Product f g b
  map f (Product fa ga) = Product (f <$> fa) (f <$> ga)

instance traversableProduct :: (Traversable f, Traversable g) => Traversable (Product f g) where
  -- Implement the traverse function for a Traversable instance for Product
  traverse :: forall f' a b. Applicative f' => (a -> f' b) -> Product f g a -> f' (Product f g b)
  traverse f (Product fa ga) = lift2 Product (traverse f fa) (traverse f ga)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a = InL (f a) | InR (g a)

instance functorCoproduct :: (Functor f, Functor g) => Functor (Coproduct f g) where
  -- Implement the (<$>) function for a Functor instance for Coproduct
  map :: forall a b. (a -> b) -> Coproduct f g a -> Coproduct f g b
  map f (InL fa) = InL $ f <$> fa
  map f (InR ga) = InR $ f <$> ga

instance traversableCoProduct :: (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  -- Implement the traverse function for a Traversable instance for Coproduct
  traverse :: forall f' a b. Applicative f' => (a -> f' b) -> Coproduct f g a -> f' (Coproduct f g b)
  traverse f (InL fa) = InL <$> traverse f fa
  traverse f (InR ga) = InR <$> traverse f ga
