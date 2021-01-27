module Course.StateTSpec where

import Course.Applicative (pure, (<*>))
import Course.ExactlyOne (ExactlyOne(..))
import Course.Functor ((<$>))
import Course.List (List(..), flatMap, listh, (:.))
import Course.Monad ((=<<), (>>=))
import Course.Optional (Optional(..))
import Course.State (put, runState)
import Course.StateT (runStateT, Logger(..), OptionalT(..), StateT(..), distinct', distinctF, distinctG, getT, log1, putT, runOptionalT, runState', state')
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Monad, Unit, const, discard, unit, ($), (+), (*), (<>))
import Test.QuickCheck (class Testable, (===), quickCheck)
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- prop :: forall m prop. MonadEffect m => Testable prop => String -> prop -> m Unit
prop :: forall t m2 m. Monad m => MonadEffect m2 => Testable t => String -> t -> SpecT m2 Unit m Unit
prop msg p = it msg (liftEffect $ quickCheck p)

spec :: Spec Unit
spec = do

  describe "Functor" $ do
    it "<$>" $ do
      let
        st =
          StateT (\s -> ((2 /\ s) :. Nil))
      runStateT (((+)1) <$> st) 0 `shouldEqual` ((3 /\ 0) :. Nil)

  describe "Applicative" $ do
    it "List (pure)" $ runStateT ((pure 2) :: StateT Int List Int) 0 `shouldEqual` ((2 /\ 0) :. Nil)
    it "List (<*>)" $ runStateT (pure ((+)2) <*> ((pure 2) :: StateT Int List Int)) 0 `shouldEqual` ((4 /\ 0) :. Nil)
    it "Optional" $ do
      let
        st =
          StateT (\s -> Full (((+)2) /\ (s <> [1]))) <*> (StateT (\s -> Full (2 /\ (s <> [2]))))
      runStateT st [0] `shouldEqual` Full (4 /\ [0,1,2])
    it "List" $ do
      let
        st =
          StateT (\s -> (((+)2) /\  (s <> [1])) :. (((+)3) /\  (s <> [1])) :. Nil)
            <*> (StateT (\s -> (2 /\  (s <> [2])) :. Nil))
      runStateT st [0] `shouldEqual` ((4 /\ [0,1,2]) :. (5 /\ [0,1,2]) :. Nil)

  describe "Monad" $ do
    it "bind const" $ do
      let
        s n =
          StateT $ const ((unit /\  n) :. Nil)
      runStateT (const (s 2) =<< s 1) 0 `shouldEqual` ((unit /\  2) :. Nil)
    it "modify" $ do
      let
        modify f =
          StateT (\s -> pure (unit /\  f s))
      runStateT (modify ((+)1) >>= \_ -> modify ((*)2)) 7
        `shouldEqual`
          ((unit /\  16) :. Nil)

  describe "state'" $ do
    it "lifts stately functions" $
      runStateT (state' $ runState $ put 1) 0 `shouldEqual` ExactlyOne (unit /\  1)

  describe "runState'" $ do
    it "runs identity states" $
      runState' (state' $ runState $ put 1) 0 `shouldEqual` (unit /\ 1)

  describe "getTTest" $ do
    it "returns it's input" $
      runStateT (getT :: StateT Int List Int) 3 `shouldEqual` ((3 /\ 3) :. Nil)

  describe "putTTest" $ do
    it "puts the state" $
      runStateT (putT 2 :: StateT Int List Unit) 0 `shouldEqual` ((unit /\ 2) :. Nil)

  describe "distinct'" $ do
    prop "removes adjacent duplicates" $
      \(xs :: List Int) ->
        distinct' xs === distinct' (flatMap (\x -> x :. x :. Nil) xs)

  describe "distinctF" $ do
    it "Full case" $
      distinctF (listh [1,2,3,2,1]) `shouldEqual` Full (listh [1,2,3])
    it "Empty case" $
      distinctF (listh [1,2,3,2,1,101]) `shouldEqual` Empty

  describe "OptionalT" $ do
    it "(<$>) for OptionalT" $
      runOptionalT (((+)1) <$> OptionalT (Full 1 :. Empty :. Nil))
        `shouldEqual`
          (Full 2 :. Empty :. Nil)

    describe "(<*>) for OptionalT" $ do
      it "one" $ do
        let
          ot =
            OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
        runOptionalT ot `shouldEqual` (Nil :: List (Optional Int))
      it "two" $ do
        let
          ot =
            OptionalT (Full ((+)1) :. Full ((+)2) :. Nil) <*> OptionalT Nil
        runOptionalT ot `shouldEqual` (Nil :: List (Optional Int))
      it "three" $ do
        let
          ot =
            OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
        runOptionalT ot `shouldEqual` (Empty :. Nil :: List (Optional Int))
      it "four" $ do
        let
          ot =
            OptionalT (Full ((+)1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
        runOptionalT ot `shouldEqual` (Empty :. Empty :. Nil :: List (Optional Int))
      it "five" $ do
        let
          ot =
            (OptionalT (Empty :. Nil)) <*> (OptionalT (Full 1 :. Full 2 :. Nil))
        runOptionalT ot `shouldEqual` (Empty :. Nil :: List (Optional Int))
      it "six" $ do
        let
          ot =
            OptionalT (Full ((+)1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
        runOptionalT ot `shouldEqual` (Full 2 :. Full 3 :. Empty :. Nil)
      it "seven" $ do
        let
          ot =
            OptionalT (Full ((+)1) :. Full ((+)2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
        runOptionalT ot `shouldEqual` (Full 2 :. Empty :. Full 3 :. Empty :. Nil)

    describe "OptionalT Monad" $ do
      it "(=<<) for OptionalT" $ do
        let
          func a =
            OptionalT (Full (a+1) :. Full (a+2) :. Nil)
          ot =
            func =<< OptionalT (Full 1 :. Empty :. Nil)
        runOptionalT ot `shouldEqual` (Full 2:.Full 3:.Empty:.Nil)

  describe "Logger" $ do
    it "(<$>) for Logger" $
      (((+)3) <$> Logger (1 :. 2 :. Nil) 3) `shouldEqual` Logger (1 :. 2 :. Nil) 6

    describe "Applicative" $ do
      it "pure" $
        (pure "table" :: Logger Int String) `shouldEqual` Logger Nil "table"
      it "<*>" $
        (Logger (1:.2:.Nil) ((+)7) <*> Logger (3:.4:.Nil) 3)
          `shouldEqual`
            Logger (1:.2:.3:.4:.Nil) 10

    describe "Monad" $ do
      it "(=<<) for Logger" $ do
        let
          func a =
            Logger (4:.5:.Nil) (a+3)
        (func =<< Logger (1:.2:.Nil) 3)
          `shouldEqual`
            Logger (1:.2:.4:.5:.Nil) 6

  it "log1" $
    log1 1 2 `shouldEqual` Logger (1:.Nil) 2

  describe "distinctG" $ do
    it "Full case" $ do
      let
        expected =
          Logger
            ("even number: 2":."even number: 2":."even number: 6":.Nil)
            (Full (1:.2:.3:.6:.Nil))
      distinctG (1:.2:.3:.2:.6:.Nil) `shouldEqual` expected
    it "Empty case" $ do
      let
        expected =
          Logger
            ("even number: 2":."even number: 2":."even number: 6":."aborting > 100: 106":.Nil)
            Empty
      distinctG (listh [1,2,3,2,6,106]) `shouldEqual` expected
