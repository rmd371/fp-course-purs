module Course.ExactlyOne where

import Prelude

data ExactlyOne a = ExactlyOne a

derive instance eqExactlyOne :: Eq a => Eq (ExactlyOne a)

instance showExactlyOne :: Show a => Show (ExactlyOne a) where
    show :: forall a. Show a => ExactlyOne a -> String
    show (ExactlyOne a) = show a

runExactlyOne :: forall a. ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

mapExactlyOne :: forall a b. (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a) = ExactlyOne (f a)

bindExactlyOne :: forall a b. (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

instance functorExactlyOne :: Functor ExactlyOne where
  map :: forall a b. (a -> b) -> ExactlyOne a -> ExactlyOne b
  map = liftM1

instance applicativeExactlyOne :: Applicative ExactlyOne where
  pure :: forall a. a -> ExactlyOne a
  pure = ExactlyOne

instance applyReader :: Apply ExactlyOne where
  apply :: forall a b. ExactlyOne (a -> b) -> ExactlyOne a -> ExactlyOne b
  apply (ExactlyOne f) (ExactlyOne r) = ExactlyOne (f r)

instance bindExactlyOne1 :: Bind ExactlyOne where
  bind :: forall a b. ExactlyOne a -> (a -> ExactlyOne b) -> ExactlyOne b
  bind = flip bindExactlyOne

instance monadExactlyOne :: Monad ExactlyOne  