module Course.ComonadSpec where

import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

import Prelude (discard, ($), (+), Unit)
import Course.Comonad    (copure, (<$$>))
import Course.ExactlyOne (ExactlyOne (..))

spec :: Spec Unit
spec = do
  it "ExactlyOne" $ copure (ExactlyOne 7) `shouldEqual` 7

  it "<$$>" $
    (((+)10) <$$> ExactlyOne 7) `shouldEqual` ExactlyOne 17