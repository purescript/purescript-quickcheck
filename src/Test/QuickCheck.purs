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
import Control.Monad.Eff.Exception (EXCEPTION(), throwException, error)
import Control.Monad.Eff.Random (RANDOM())
import Data.List (List(..), replicateM)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.LCG

-- | A type synonym which represents the effects used by the `quickCheck` function.
type QC eff a = Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) a

-- | Test a property.
-- |
-- | This function generates a new random seed, runs 100 tests and
-- | prints the test results to the console.
quickCheck :: forall eff prop. (Testable prop) => prop -> QC eff Unit
quickCheck prop = quickCheck' 100 prop

-- | A variant of the `quickCheck` function which accepts an extra parameter
-- | representing the number of tests which should be run.
quickCheck' :: forall eff prop. (Testable prop) => Int -> prop -> QC eff Unit
quickCheck' n prop = do
  seed <- randomSeed
  let results = quickCheckPure seed n prop
  let successes = countSuccesses results
  log $ show successes ++ "/" ++ show n ++ " test(s) passed."
  throwOnFirstFailure one results

  where

  throwOnFirstFailure :: Int -> List Result -> QC eff Unit
  throwOnFirstFailure _ Nil = return unit
  throwOnFirstFailure n (Cons (Failed msg) _) = throwException $ error $ "Test " ++ show n ++ " failed: \n" ++ msg
  throwOnFirstFailure n (Cons _ rest) = throwOnFirstFailure (n + one) rest

  countSuccesses :: List Result -> Int
  countSuccesses Nil = zero
  countSuccesses (Cons Success rest) = one + countSuccesses rest
  countSuccesses (Cons _ rest) = countSuccesses rest

-- | Test a property, returning all test results as an array.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
quickCheckPure :: forall prop. (Testable prop) => Seed -> Int -> prop -> List Result
quickCheckPure s n prop = evalGen (replicateM n (test prop)) { newSeed: s, size: 10 }

-- | The `Testable` class represents _testable properties_.
-- |
-- | A testable property is a function of zero or more `Arbitrary` arguments,
-- | returning a `Boolean` or `Result`.
-- |
-- | Testable properties can be passed to the `quickCheck` function.
class Testable prop where
  test :: prop -> Gen Result

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = arbitrary >>= test <<< f

-- | The result of a test: success or failure (with an error message).
data Result = Success | Failed String

instance showResult :: Show Result where
  show Success = "Success"
  show (Failed msg) = "Failed: " ++ msg

-- | This operator attaches an error message to a failed test.
-- |
-- | For example:
-- |
-- | ```purescript
-- | test x = myProperty x <?> ("myProperty did not hold for " <> show x)
-- | ```
(<?>) :: Boolean -> String -> Result
(<?>) true _ = Success
(<?>) false msg = Failed msg

-- | Self-documenting equality assertion
(===) :: forall a. (Eq a, Show a) => a -> a -> Result
(===) a b = a == b <?> show a ++ " /= " ++ show b

-- | Self-documenting inequality assertion
(/==) :: forall a. (Eq a, Show a) => a -> a -> Result
(/==) a b = a /= b <?> show a ++ " == " ++ show b
