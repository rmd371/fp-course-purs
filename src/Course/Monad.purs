module Course.Monad where

import Course.Applicative (class Applicative)
import Course.ExactlyOne (ExactlyOne(..))
import Course.Functor (map)
import Course.List (List(..))
import Course.Optional (Optional(..))
import Prelude (unit)
import Unsafe.Coerce (unsafeCoerce)

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f <= Monad f where
  bind :: forall a b. (a -> f b) -> f a -> f b

infixl 1 bind as =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance exactlyOneMonad :: Monad ExactlyOne where
  bind :: forall a b. (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
  bind f fa = ExactlyOne (unsafeCoerce unit) --error "todo: Course.Monad (=<<)#instance ExactlyOne"

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance monadList :: Monad List where
  bind :: forall a b. (a -> List b) -> List a -> List b
  bind f fa = Nil --error "todo: Course.Monad bind#instance List"

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance optionalMonad :: Monad Optional where
  bind :: forall a b. (a -> Optional b) -> Optional a -> Optional b
  bind f fa = Empty --error "todo: Course.Monad (=<<)#instance Optional"

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance functionMonad :: Monad ((->) t) where
  bind :: forall t a b. (a -> ((->) t b)) -> ((->) t a) -> ((->) t b)
  bind f fa = \_ -> (unsafeCoerce unit) -- error "todo: Course.Monad (=<<)#instance ((->) t)"

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
apply :: forall f a b. Monad f => f (a -> b) -> f a -> f b
apply f fa = map (\_ -> unsafeCoerce "todo: Course.Monad#(<**>)") fa 

infixl 4 apply as <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join :: forall f a b. Monad f => f (f a) -> f a
join ffa = map (\_ -> unsafeCoerce "todo: Course.Monad#join") ffa

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
bindFlipped :: forall f a b. Monad f => f a -> (a -> f b) -> f b
bindFlipped fa ffb = map (\_ -> unsafeCoerce "todo: Course.Monad#(>>=)") fa

infixl 1 bindFlipped as >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
composeKleisliFlipped ::
  forall f a b c. Monad f => (b -> f c) -> (a -> f b) -> a -> f c
composeKleisliFlipped ffc ffb a = map (\f -> unsafeCoerce "todo: Course.Monad#(<=<)") (ffb a)

infixr 1 composeKleisliFlipped as <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- instance ioMonad :: Monad IO where
--   bind =
--     (P.(=<<))
