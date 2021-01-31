module Course.Compose where

import Course.Applicative
import Course.Core
import Course.Functor

import Course.Functor (map) as F
import Course.Monad (class Monad, bind, (>>=))
import Prelude (identity, ($), (<<<))

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map :: forall a b. (a -> b) -> Compose f g a -> Compose f g b
  map f (Compose fga) = Compose $ map f <$> fga

instance applicativeCompose :: (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- Implement the pure function for an Applicative instance for Compose
  pure :: forall a. a -> Compose f g a
  pure = Compose <<< pure <<< pure
  -- Implement the (<*>) function for an Applicative instance for Compose
  apply :: forall a b. Compose f g (a -> b) -> Compose f g a -> Compose f g b
  apply (Compose fgf) (Compose fga) = Compose $ lift2 apply fgf fga

instance monadCompose :: (Monad f, Monad g) => Monad (Compose f g) where
  -- Implement the (=<<) function for a Monad instance for Compose
  bind :: forall a b. (a -> Compose f g b) -> Compose f g a -> Compose f g b
  -- bind f (Compose fga) = Compose $ bind (\a -> (\(Compose fgb) -> fgb) (f a)) fga
  bind f (Compose fga) = error "Impossible"
