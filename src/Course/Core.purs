module Course.Core where
  
import Prelude
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

error :: forall a. String -> a
error msg =
  unsafePerformEffect
    $ do
        log msg
        pure $ unsafeCoerce unit

-- module Course.Core(
--   Eq(..)
-- , Ord(..)
-- , Show(..)
-- , Integral(..)
-- , RealFrac(..)
-- , Num(..)
-- , Fractional(..)
-- , Bool(..)
-- , Either(..)
-- , Int
-- , Integer
-- , IO
-- , Rational
-- , seq
-- , error
-- , undefined
-- , const
-- , flip
-- , curry
-- , uncurry
-- , id
-- , otherwise
-- --, (.)
-- , ($)
-- , (&&)
-- , (||)
-- , not
-- , even
-- , odd
-- , fst
-- , snd
-- , getChar
-- , on
-- , first
-- , second
-- , (&&&)
-- , (***)
-- , IsString(..)
-- , module Data.Char
-- , ifThenElse
-- , bool
-- ) where


-- import Prelude(
--     Eq(..)
--   , Ord(..)
--   , Show(..)
--   , Integral(..)
--   , RealFrac(..)
--   , Num(..)
--   , Fractional(..)
--   , Bool(..)
--   , Either(..)
--   , Char
--   , Int
--   , Integer
--   , IO
--   , Rational
--   , seq
--   , error
--   , undefined
--   , const
--   , flip
--   , curry
--   , uncurry
--   , id
--   , otherwise
--   , (.)
--   , ($)
--   , (&&)
--   , (||)
--   , not
--   , even
--   , odd
--   , fst
--   , snd
--   )
-- import Data.String(
--   IsString(..)
--   )

-- import System.IO(
--     getChar
--   )
-- import Data.Function(
--     on
--   )
-- import Control.Arrow(
--     first
--   , second
--   , (&&&)
--   , (***)
--   )
-- import Data.Char

ifThenElse :: forall a. Boolean -> a -> a -> a
ifThenElse true t _ = t
ifThenElse false _ f = f

-- bool ::
--   a
--   -> a
--   -> Bool
--   -> a
-- bool f _ False =
--   f
-- bool _ t True =
--   t
