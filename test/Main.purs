module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array.Partial (head)
import Data.Foldable (sum)

import Partial.Unsafe (unsafePartial)

import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf, randomSample')

main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  log "Try with some little Gens first"
  logShow =<< go 10
  logShow =<< go 100
  logShow =<< go 1000
  logShow =<< go 10000

  log "Testing stack safety of Gen"
  logShow =<< go 20000
  logShow =<< go 100000

  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (arbitrary :: Gen Int))

  unsafeHead :: forall x. Array x -> x
  unsafeHead xs = unsafePartial (head xs)
