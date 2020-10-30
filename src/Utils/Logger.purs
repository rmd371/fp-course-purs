module Logger where

import Prelude

import Course.List (List, hlist, listh)

foreign import mylog :: forall a. String -> a -> a

logShow :: forall a. Show a => a -> a
logShow a = mylog (show a) a
