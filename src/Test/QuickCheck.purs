module Test.QuickCheck where

import Debug.Trace
import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Data.Array
import Data.Tuple
import Data.Maybe
import Data.Either
import Math

import qualified Data.Char as S
import qualified Data.String as S
import qualified Data.String.Unsafe as SU

import Test.QuickCheck.Gen

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

instance arbChar :: Arbitrary S.Char where
  arbitrary = S.fromCharCode <<< ((*) 65535) <$> uniform

instance coarbChar :: CoArbitrary S.Char where
  coarbitrary c = coarbitrary $ S.toCharCode c

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
  arbitrary = S.fromCharArray <$> arbitrary

instance coarbString :: CoArbitrary String where
  coarbitrary s = coarbitrary $ (S.charCodeAt 0 <$> S.split "" s)

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ AlphaNumString <<< S.fromCharArray $ lookup <$> arrNum where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      lookup x = SU.charAt index chars where
        index = round $ x * (S.length chars - 1)

instance coarbAlphaNumString :: CoArbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1
  coarbitrary (Just a) = coarbitrary a

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f xs) gen

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
quickCheckPure s = quickCheckPure' {newSeed: s, size: 10} where
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

quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' 100 prop

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
