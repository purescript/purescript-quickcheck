module Test.QuickCheck where

import Prelude
import Data.Array
import Data.Maybe
import Data.Either
import Data.Tuple
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random

data Result = Success | Failed String

type QC = forall eff. Eff (random :: Random, trace :: Trace, err :: Exception String | eff) {}

class (Show t) <= Arb t where
  arb :: forall eff. Eff (random :: Random | eff) t

instance arbNumber :: Arb Number where
  arb = random 

instance arbBoolean :: Arb Boolean where
  arb = do
    n <- random
    return ((n * 2) < 1)

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
  test :: forall eff. prop -> Eff (random :: Random | eff) Result

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

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC
quickCheck' n prop = run 1 prop n
  where
  run 2 _ 1 = trace $ "Test passed" 
  run n _ t | n > t = trace $ show t ++ " tests passed" 
  run n prop t = do
    result <- test prop
    case result of
      Success -> run (n + 1) prop t
      Failed msg -> throwException $ "Test " ++ show n ++ " failed: \n" ++ msg

quickCheck :: forall prop. (Testable prop) => prop -> QC
quickCheck prop = quickCheck' 100 prop
