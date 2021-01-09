module Course.Optional where

import Control.Applicative as A
import Control.Apply as Ap
import Control.Monad as M
import Prelude (class Show, class Eq, show, (<<<), (==))
import Prelude as P
import Data.Function (flip)

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a = Full a | Empty

derive instance optionalExactlyOne :: Eq a => Eq (Optional a)

instance showExactlyOne :: Show a => Show (Optional a) where
    show :: forall a. Show a => Optional a -> String
    show (Full a) = show a
    show Empty = ""

-- -- | Map the given function on the possible value.
-- --
-- -- >>> mapOptional (+1) Empty
-- -- Empty
-- --
-- -- >>> mapOptional (+1) (Full 8)
-- -- Full 9
mapOptional :: forall a b. (a -> b) -> Optional a -> Optional b
mapOptional f (Full a) = Full (f a)
mapOptional _  Empty   = Empty

-- -- | Bind the given function on the possible value.
-- --
-- -- >>> bindOptional Full Empty
-- -- Empty
-- --
-- -- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- -- Full 7
-- --
-- -- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- -- Full 10
bindOptional :: forall a b. (a -> Optional b) -> Optional a -> Optional b
bindOptional f (Full a) = f a
bindOptional _  Empty   = Empty

-- -- | Return the possible value if it exists; otherwise, the second argument.
-- --
-- -- >>> Full 8 ?? 99
-- -- 8
-- --
-- -- >>> Empty ?? 99
-- -- 99
optional :: forall a. Optional a -> a -> a
optional (Full a) _  = a
optional  Empty   a' = a'

infixl 12 optional as ??     

-- -- | Try the first optional for a value. If it has a value, use it; otherwise,
-- -- use the second value.
-- --
-- -- >>> Full 8 <+> Empty
-- -- Full 8
-- --
-- -- >>> Full 8 <+> Full 9
-- -- Full 8
-- --
-- -- >>> Empty <+> Full 9
-- -- Full 9
-- --
-- -- >>> Empty <+> Empty
-- -- Empty
eitherOptional :: forall a. Optional a -> Optional a -> Optional a
eitherOptional Empty    m   = m
eitherOptional (Full a) _   = Full a

infixl 12 eitherOptional as <+>

applyOptional :: forall a b. Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: forall a b c. (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional <<< mapOptional f

contains :: forall a. Eq a => a -> Optional a -> Boolean
contains _ Empty = false
contains a (Full z) = a == z

instance functorOptional :: P.Functor Optional where
  map :: forall a b. (a -> b) -> Optional a -> Optional b
  map = mapOptional

instance applyOptional' :: Ap.Apply Optional where
  apply :: forall a b. Optional (a -> b) -> Optional a -> Optional b
  apply = applyOptional

instance applicativeOptional :: A.Applicative Optional where
  pure = Full

instance monadOptional :: M.Bind Optional where
  bind :: forall a b. Optional a -> (a -> Optional b) -> Optional b
  bind = flip bindOptional
