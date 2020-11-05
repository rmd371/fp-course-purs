module Logger where

import Prelude

foreign import mylog :: forall a. String -> a -> a
foreign import error :: forall a. String -> a

logShow :: forall a. Show a => a -> a
logShow a = mylog (show a) a
