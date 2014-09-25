module Test.QuickCheck where

import Debug.Trace
import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Data.Array
import Math

import qualified Data.String as S

import Test.QuickCheck.LCG

newtype AlphaNumString = AlphaNumString String

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
  coarbitrary true = perturbGen 1
  coarbitrary false = perturbGen 2

instance arbString :: Arbitrary String where
  arbitrary = do
    arrNum <- arbitrary
    return $ (S.joinWith "") $ S.fromCharCode <<< ((*) 65535) <$> arrNum

instance coarbString :: CoArbitrary String where
  coarbitrary s = coarbitrary $ (S.charCodeAt 0 <$> S.split "" s)

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ AlphaNumString <<< (S.joinWith "") $ lookup <$> arrNum where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      lookup x = S.charAt index chars where
        index = round $ x * (S.length chars - 1)

instance coarbAlphaNumString :: CoArbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

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
quickCheckPure s = quickCheckPure' {newSeed: s, size: 10}

quickCheckPure' :: forall prop. (Testable prop) => GenState -> Number -> prop -> [Result]
quickCheckPure' st n prop = evalGen (go n) st 
  where
  go n | n <= 0 = return []
  go n = do
    result <- test prop
    rest <- go (n - 1)
    return $ result : rest

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception | eff) a

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
quickCheck' n prop = do
  seed <- randomSeed
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

quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' 100 prop

foreign import randomSeed
  "function randomSeed() {\
  \  return Math.floor(Math.random() * (1 << 30));\
  \}" :: forall eff. Eff (random :: Random | eff) Number
