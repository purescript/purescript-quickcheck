module Test.QuickCheck where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.Eff.Unsafe

import Data.Array
import Data.Maybe
import Data.Either
import Data.Tuple

import Debug.Trace

import Math

data Result = Success | Failed String

type Gen a = forall eff. Eff (random :: Random | eff) a

data Nat = Nat Number

unNat :: Nat -> Number
unNat (Nat n) = n

class Arb t where
  arb :: Gen t

instance arbNumber :: Arb Number where
  arb = random

instance arbNat :: Arb Nat where
  arb = do
    n <- random
    return (Nat (floor (n * 100)))

instance arbBoolean :: Arb Boolean where
  arb = do
    n <- random
    return ((n * 2) < 1)

instance arbString :: Arb String where
  arb = do
    codes <- arb :: Gen [Nat]
    let ascii = filter (\(Nat n) -> 32 <= n && n <= 127) codes
    return $ joinWith (fromCharCode <<< unNat <$> ascii) ""

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

class Testable prop where
  test :: prop -> Gen Result

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Show t, Arb t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arb
    result <- test (f t)
    case result of
      Success -> return Success
      Failed msg -> return $ Failed $ "Failed on input " ++ show t ++ ": \n" ++ msg

quickCheck' :: forall prop eff. (Testable prop) => Number -> prop -> Eff (random :: Random, trace :: Trace, err :: Exception String | eff) {}
quickCheck' 0 _ = trace "All tests passed"
quickCheck' n prop = do
  result <- test prop
  case result of
    Success -> quickCheck' (n - 1) prop
    Failed msg -> throwException $ "Test failed: \n" ++ msg

quickCheck :: forall prop eff. (Testable prop) => prop -> Eff (random :: Random, trace :: Trace, err :: Exception String | eff) {}
quickCheck prop = quickCheck' 100 prop

choose :: Tuple Number Number -> Number
choose (Tuple low high) = randomRange low high

oneof :: forall a. [a] -> a
oneof xs = let n = choose (Tuple 0 (length xs - 1)) in xs !! n

elements :: forall a. [a] -> Gen a
elements xs = oneof (retEff <$> xs)

sized :: forall a. (Number -> Gen a) -> Gen  a
sized f = random >>= f

resize :: forall a. Number -> Gen a -> Gen  a
resize n x = sized (\n -> x)

foreign import randomRange
  "function randomRange(low) {\
  \  return function(high) {\
  \    return Math.floor(Math.random() * (high - low + 1)) + low;\
  \  }\
  \}" :: Number -> Number -> Number

foreign import fromCharCode
  "function fromCharCode(n) {\
  \  return String.fromCharCode(n);\
  \}" :: Number -> String
