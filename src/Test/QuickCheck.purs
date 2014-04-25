module Test.QuickCheck where

{- 
import Data.Array
import Data.Maybe
import Data.Either
import Data.Tuple
-}

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception

import Test.QuickCheck.LCG

class (Show t) <= Arbitrary t where
  arbitrary :: Gen t

class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

data Result = Success | Failed String

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform 

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return ((n * 2) < 1)

instance arbFunction :: (Show (a -> b), CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = liftGen (\a -> coarbitrary a arbitrary)

liftGen :: forall a b. (a -> Gen b) -> Gen (a -> b)
liftGen f = Gen $ \l -> { value: \a -> (runGen (f a) l).value, newSeed: l }

{-
instance arbArray :: (Arb a) => Arb [a] where
  arb = do
    b <- arb
    if b then return [] else do
      a <- arb
      as <- arb
      return (a : as)

instance arbMaybe :: (Arb a) => Arb (Maybe a) where
  arb = do
    b <- arb
    if b then pure Nothing else Just <$> arb

instance arbEither :: (Arb a, Arb b) => Arb (Either a b) where
  arb = do
    b <- arb
    if b then Left <$> arb else Right <$> arb

instance arbTuple :: (Arb a, Arb b) => Arb (Tuple a b) where
  arb = Tuple <$> arb <*> arb
-}

class Testable prop where
  test :: prop -> Gen Result

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Show t, Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arbitrary
    result <- test (f t)
    case result of
      Success -> return Success
      Failed msg -> return $ Failed $ "Failed on input " ++ show t ++ ": \n" ++ msg

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception String | eff) a

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC {}
quickCheck' n prop = run 1 prop n
  where
  run 2 _ 1 = trace $ "Test passed" 
  run n _ t | n > t = trace $ show t ++ " tests passed" 
  run n prop t = do
    result <- runGenWithRandomSeed $ test prop
    case result of
      Success -> run (n + 1) prop t
      Failed msg -> throwException $ "Test " ++ show n ++ " failed: \n" ++ msg

quickCheck :: forall prop. (Testable prop) => prop -> QC {}
quickCheck prop = quickCheck' 100 prop
