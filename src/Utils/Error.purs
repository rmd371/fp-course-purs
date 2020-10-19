module Utils.Error where

import Prelude

import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw
