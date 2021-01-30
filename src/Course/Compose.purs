module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map :: forall f g a b. (a -> b) -> Compose f g a -> Compose f g b
  map = error "todo: Course.Compose (<$>)#instance (Compose f g)"

instance applicativeCompose :: (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- Implement the pure function for an Applicative instance for Compose
  pure :: forall f g a. a -> Compose f g a
  pure = error "todo: Course.Compose pure#instance (Compose f g)"
  -- Implement the (<*>) function for an Applicative instance for Compose
  apply :: forall f g a b. Compose f g (a -> b) -> Compose f g a -> Compose f g b
  apply = error "todo: Course.Compose (<*>)#instance (Compose f g)"

instance monadCompose :: (Monad f, Monad g) => Monad (Compose f g) where
  -- Implement the (=<<) function for a Monad instance for Compose
  bind :: forall f g a b. (a -> Compose f g b) -> Compose f g a -> Compose f g b
  bind = error "todo: Course.Compose (<<=)#instance (Compose f g)"