module Course.ExactlyOne where

import Prelude
-- import Control.Applicative as A
import Control.Monad as M

data ExactlyOne a = ExactlyOne a --deriving (Eq, Show)

--derive instance exactlyOneEq :: Eq (ExactlyOne a)
--derive instance exactlyOneShow :: Show a => Show (ExactlyOne a)

instance showExactlyOne :: Show a => Show (ExactlyOne a) where
    show (ExactlyOne a) = show a

runExactlyOne :: forall a. ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

mapExactlyOne :: forall a b. (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a)    = ExactlyOne (f a)

bindExactlyOne :: forall a b. (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

instance functorExactlyOne :: Functor ExactlyOne where
  map = M.liftM1

instance applicativeExactlyOne :: Applicative ExactlyOne where
  pure = ExactlyOne

instance applyReader :: Apply ExactlyOne where
  -- apply :: forall a b. f (a -> b) -> f a -> f b
  apply (ExactlyOne f) (ExactlyOne r) = ExactlyOne (f r)

instance bindExactlyOne1 :: Bind ExactlyOne where
--   (>>=) =
--     flip bindExactlyOne
  bind = flip bindExactlyOne

instance monadExactlyOne :: Monad ExactlyOne  