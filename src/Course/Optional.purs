module Course.Optional where

import Prelude

import Utils.Error (error)

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
mapOptional = error "todo: Course.Optional#mapOptional"

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
-- bindOptional :: forall a b. (a -> Optional b) -> Optional a -> Optional b
-- bindOptional = error "todo: Course.Optional#bindOptional"

-- -- | Return the possible value if it exists; otherwise, the second argument.
-- --
-- -- >>> Full 8 ?? 99
-- -- 8
-- --
-- -- >>> Empty ?? 99
-- -- 99
-- optional :: forall a. Optional a -> a -> a
-- optional = error "todo: Course.Optional#(??)"

-- infixl 12 optional as ??     

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
-- eitherOptional :: forall a. Optional a -> Optional a -> Optional a
-- eitherOptional = error "todo: Course.Optional#(<+>)"  

-- infixl 12 eitherOptional as <+>

-- applyOptional :: Optional (a -> b) -> Optional a -> Optional b
-- applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

-- twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
-- twiceOptional f = applyOptional . mapOptional f

-- contains :: Eq a => a -> Optional a -> Bool
-- contains _ Empty = False
-- contains a (Full z) = a == z

-- instance P.Functor Optional where
--   fmap =
--     M.liftM

-- instance A.Applicative Optional where
--   (<*>) =
--     M.ap
--   pure =
--     Full

-- instance P.Monad Optional where
--   (>>=) =
--     flip bindOptional
--   return =
--     Full