module Test.QuickCheck where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception

import Test.QuickCheck.LCG

class Arbitrary t where
  arbitrary :: Gen t

class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

data Result = Success | Failed String

instance showResult :: Show Result where
  show Success = "Success"
  show (Failed msg) = "Failed: " ++ msg

(<?>) :: Boolean -> String -> Result
(<?>) true _ = Success
(<?>) false msg = Failed msg

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform 

instance coarbNumber :: CoArbitrary Number where
  coarbitrary = perturbGen  

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n * 2) < 1

instance coarbBoolean :: CoArbitrary Boolean where
  coarbitrary true (Gen f) = Gen $ \l -> f (l + 1)
  coarbitrary false (Gen f) = Gen $ \l -> f (l + 2)

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ \l -> { value: \a -> (runGen (f a) l).value, newSeed: l }

instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f xs) gen
    where
    map _ [] = []
    map f (x : xs) = f x : map f xs

instance arbArray :: (Arbitrary a) => Arbitrary [a] where
  arbitrary = do
    b <- arbitrary
    if b then return [] else do
      a <- arbitrary
      as <- arbitrary
      return (a : as)

instance coarbArray :: (CoArbitrary a) => CoArbitrary [a] where
  coarbitrary [] = id
  coarbitrary (x : xs) = coarbitrary xs <<< coarbitrary x

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

quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]
quickCheckPure seed n prop = evalGen (go n) seed
  where
  go n | n <= 0 = return []
  go n = do
    result <- test prop
    rest <- go (n - 1)
    return $ result : rest

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception String | eff) a

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC {}
quickCheck' n prop = do
  seed <- randomSeed
  let results = quickCheckPure seed n prop
  let successes = countSuccesses results
  trace $ show successes ++ "/" ++ show n ++ " test(s) passed."
  throwOnFirstFailure 1 results

  where

  throwOnFirstFailure :: Number -> [Result] -> QC {}
  throwOnFirstFailure _ [] = return {}
  throwOnFirstFailure n (Failed msg : _) = throwException $ "Test " ++ show n ++ " failed: \n" ++ msg
  throwOnFirstFailure n (_ : rest) = throwOnFirstFailure (n + 1) rest

  countSuccesses :: [Result] -> Number
  countSuccesses [] = 0
  countSuccesses (Success : rest) = 1 + countSuccesses rest
  countSuccesses (_ : rest) = countSuccesses rest

quickCheck :: forall prop. (Testable prop) => prop -> QC {}
quickCheck prop = quickCheck' 100 prop
