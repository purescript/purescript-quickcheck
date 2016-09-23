-- | This module is a partial port of the Haskell QuickCheck library.
-- |
-- | QuickCheck provides a way to write _property-based_ tests.
-- |
-- | The `Arbitrary` and `CoArbitrary` type classes allow us to create
-- | random data with which we can run our tests. This module provides
-- | instances of both classes for PureScript's core data structures,
-- | as well as functions for writing new instances.
-- |
-- | Test suites can use the `quickCheck` and `quickCheckPure` functions
-- | to test properties.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = quickCheck \n -> n + 1 > n
-- | ```
module Test.QuickCheck where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Rec.Class (tailRecM)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, for_)
import Data.List (List)
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..), runFirst)
import Data.Monoid (mempty)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (evalGen, runGen)
import Test.QuickCheck.LCG (Seed, randomSeed)

-- | A type synonym which represents the effects used by the `quickCheck` function.
type QC eff a = Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) a

-- | Test a property.
-- |
-- | This function generates a new random seed, runs 100 tests and
-- | prints the test results to the console.
quickCheck :: forall eff prop. Testable prop => prop -> QC eff Unit
quickCheck prop = quickCheck' 100 prop

-- | A variant of the `quickCheck` function which accepts an extra parameter
-- | representing the number of tests which should be run.
quickCheck' :: forall eff prop. Testable prop => Int -> prop -> QC eff Unit
quickCheck' n prop = do
  runSpace (test :: Space prop) \try -> do
    let
      loop { seed, index, successes, firstFailure }
        | index == n = pure (Right { successes, firstFailure })
        | otherwise =
            case runGen arbitrary { newSeed: seed, size: 10 } of
              Tuple testCase { newSeed } -> do
                case try prop testCase of
                  Success ->
                    pure (Left { seed: newSeed
                               , index: index + 1
                               , successes: successes + 1
                               , firstFailure
                               })
                  Failed message ->
                    pure (Left { seed: newSeed
                               , index: index + 1
                               , successes
                               , firstFailure: firstFailure <> First (Just { index, message, testCase })
                               })

      search { shrinkCount, testCase, index, message } = do
        case runFirst (foldMap (First <<< tryShrink) (shrink testCase)) of
          Just { message: message', testCase: testCase' } | shrinkCount < 100 -> do
            search { index, message: message', shrinkCount: shrinkCount + 1, testCase: testCase' }
          _ ->
            log $ fold
              [ "Test "
              , show (index + 1)
              , " failed (with "
              , show shrinkCount
              , " reductions): \n"
              , message
              ]

      tryShrink testCase =
        case try prop testCase of
          Success -> Nothing
          Failed message -> Just { message, testCase }

    seed <- randomSeed
    { successes, firstFailure } <- tailRecM loop { seed, index: 0, successes: 0, firstFailure: mempty }
    log $ show successes <> "/" <> show n <> " test(s) passed."
    for_ firstFailure \{ index, message, testCase } ->
      search { index, testCase, message, shrinkCount: 0 }

-- | Test a property, returning all test results as an array.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
quickCheckPure :: forall prop. Testable prop => Seed -> Int -> prop -> List Result
quickCheckPure s n prop = runSpace (test :: Space prop) \f ->
  evalGen (replicateA n (f prop <$> arbitrary)) { newSeed: s, size: 10 }

newtype SpaceF state = SpaceF
  { current :: state
  , move :: state -> LL.List state
  , test :: state -> Result
  }

newtype Space prop = Space (forall r. (forall a. Arbitrary a => (prop -> a -> Result) -> r) -> r)

runSpace :: forall prop r. Space prop -> (forall a. Arbitrary a => (prop -> a -> Result) -> r) -> r
runSpace (Space f) g = f g

space :: forall prop a. Arbitrary a => (prop -> a -> Result) -> Space prop
space f = Space \toR -> toR f

-- | The `Testable` class represents _testable properties_.
-- |
-- | A testable property is a function of zero or more `Arbitrary` arguments,
-- | returning a `Boolean` or `Result`.
-- |
-- | Testable properties can be passed to the `quickCheck` function.
class Testable prop where
  test :: Space prop

instance testableResult :: Testable Result where
  test = space \result (_ :: Unit) -> result

instance testableBoolean :: Testable Boolean where
  test = space \success (_ :: Unit) ->
    if success
      then Success
      else Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test = runSpace (test :: Space prop) \f ->
    space \g (Tuple t a) -> f (g t) a

-- | The result of a test: success or failure (with an error message).
data Result = Success | Failed String

instance showResult :: Show Result where
  show Success = "Success"
  show (Failed msg) = "Failed: " <> msg

-- | This operator attaches an error message to a failed test.
-- |
-- | For example:
-- |
-- | ```purescript
-- | test x = myProperty x <?> ("myProperty did not hold for " <> show x)
-- | ```
withHelp :: Boolean -> String -> Result
withHelp true _ = Success
withHelp false msg = Failed msg

infix 2 withHelp as <?>

-- | Self-documenting equality assertion
assertEquals :: forall a. (Eq a, Show a) => a -> a -> Result
assertEquals a b = a == b <?> show a <> " /= " <> show b

infix 2 assertEquals as ===

-- | Self-documenting inequality assertion
assertNotEquals :: forall a. (Eq a, Show a) => a -> a -> Result
assertNotEquals a b = a /= b <?> show a <> " == " <> show b

infix 2 assertNotEquals as /==
