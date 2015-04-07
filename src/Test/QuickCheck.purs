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

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Exception(), throwException, error)
import Control.Monad.Eff.Random (Random(), random)
import Data.Int (Int(), fromNumber, toNumber)
import Debug.Trace (Trace(), trace)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | A type synonym which represents the effects used by the `quickCheck` function.
type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception | eff) a

-- | Test a property.
-- |
-- | This function generates a new random seed, runs 100 tests and
-- | prints the test results to the console.
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' (fromNumber 100) prop

-- | A variant of the `quickCheck` function which accepts an extra parameter
-- | representing the number of tests which should be run.
quickCheck' :: forall prop. (Testable prop) => Int -> prop -> QC Unit
quickCheck' n prop = do
  seed <- fromNumber <<< (1073741824 *) <$> random
  let results = quickCheckPure seed n prop
  let successes = countSuccesses results
  trace $ show (toNumber successes) ++ "/" ++ show (toNumber n) ++ " test(s) passed."
  throwOnFirstFailure one results

  where

  throwOnFirstFailure :: Int -> [Result] -> QC Unit
  throwOnFirstFailure _ [] = return unit
  throwOnFirstFailure n (Failed msg : _) = throwException $ error $ "Test " ++ show (toNumber n) ++ " failed: \n" ++ msg
  throwOnFirstFailure n (_ : rest) = throwOnFirstFailure (n + one) rest

  countSuccesses :: [Result] -> Int
  countSuccesses [] = zero
  countSuccesses (Success : rest) = one + countSuccesses rest
  countSuccesses (_ : rest) = countSuccesses rest

-- | Test a property, returning all test results as an array.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
quickCheckPure :: forall prop. (Testable prop) => Int -> Int -> prop -> [Result]
quickCheckPure s = quickCheckPure' { newSeed: s, size: fromNumber 10 } where
  quickCheckPure' st n prop = evalGen (go n) st
    where
    go n | n <= zero = return []
    go n = do
      result <- test prop
      rest <- go (n - one)
      return $ result : rest

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
  test f = do
    t <- arbitrary
    test (f t)

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
(===) :: forall a b. (Eq a, Show a) => a -> a -> Result
(===) a b = a == b <?> msg
  where
    msg = show a ++ " /= " ++ show b

-- | Self-documenting inequality assertion
(/==) :: forall a b. (Eq a, Show a) => a -> a -> Result
(/==) a b = a /= b <?> msg
  where
    msg = show a ++ " == " ++ show b
