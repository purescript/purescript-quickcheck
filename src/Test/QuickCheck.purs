module Test.QuickCheck where

import Debug.Trace
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (Random(), random)
import Control.Monad.Eff.Exception (Exception(), throwException, error)

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Testable

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception | eff) a

quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' 100 prop

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
quickCheck' n prop = do
  seed <- random
  let results = quickCheckPure seed n prop
  let successes = countSuccesses results
  trace $ show successes ++ "/" ++ show n ++ " test(s) passed."
  throwOnFirstFailure 1 results

  where

  throwOnFirstFailure :: Number -> [Result] -> QC Unit
  throwOnFirstFailure _ [] = return unit
  throwOnFirstFailure n (Failed msg : _) = throwException $ error $ "Test " ++ show n ++ " failed: \n" ++ msg
  throwOnFirstFailure n (_ : rest) = throwOnFirstFailure (n + 1) rest

  countSuccesses :: [Result] -> Number
  countSuccesses [] = 0
  countSuccesses (Success : rest) = 1 + countSuccesses rest
  countSuccesses (_ : rest) = countSuccesses rest

quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]
quickCheckPure s = quickCheckPure' {newSeed: s, size: 10} where
  quickCheckPure' st n prop = evalGen (go n) st
    where
    go n | n <= 0 = return []
    go n = do
      result <- test prop
      rest <- go (n - 1)
      return $ result : rest

-- | Creates a `Result` based on a boolean with a potential failure message for
-- | when the boolean is `false`.
(<?>) :: Boolean -> String -> Result
(<?>) true _ = Success
(<?>) false msg = Failed msg

-- | Self-documenting equality assertion
(===) :: forall a b. (Eq a, Show a) => a -> a -> Result
(===) a b = a == b <?> show a ++ " /= " ++ show b

-- | Self-documenting inequality assertion
(/==) :: forall a b. (Eq a, Show a) => a -> a -> Result
(/==) a b = a /= b <?> show a ++ " == " ++ show b
