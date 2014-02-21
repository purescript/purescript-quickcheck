module QuickCheck where

import Prelude
import Data.Array
import Debug.Trace
import Control.Monad.Eff
import Random

data Result = Success | Failed String

class Arb t where
  arb :: forall eff. Eff (random :: Random | eff) t

instance Arb Number where
  arb = random 

instance Arb Boolean where
  arb = do
    n <- random
    return ((n * 2) < 1)

instance (Arb a) => Arb [a] where
  arb = do
    b <- arb
    if b then return [] else do
      a <- arb
      as <- arb
      return (a : as)

class Testable prop where
  test :: forall eff. prop -> Eff (random :: Random | eff) Result

instance Testable Result where
  test = return

instance Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance (Show t, Arb t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arb
    result <- test (f t)
    case result of
      Success -> return Success
      Failed msg -> return $ Failed $ "Failed on input " ++ show t ++ ": \n" ++ msg

quickCheck' :: forall prop eff. (Testable prop) => Number -> prop -> Eff (random :: Random, trace :: Trace | eff) {}
quickCheck' 0 _ = trace "All tests passed" 
quickCheck' n prop = do
  result <- test prop
  case result of
    Success -> quickCheck' (n - 1) prop
    Failed msg -> trace $ "Test failed: \n" ++ msg

quickCheck :: forall prop eff. (Testable prop) => prop -> Eff (random :: Random, trace :: Trace | eff) {}
quickCheck prop = quickCheck' 100 prop
