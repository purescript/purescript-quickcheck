
module Test.Main where

import Prelude
import Data.Foldable
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad.Eff.Console

main = do
  log "Try with some little Gens first"
  print $ go 10
  print $ go 100
  print $ go 1000
  print $ go 10000

  log "Testing stack safety of Gen"
  print $ go 20000

  where
  go n = sum $ _.value $ runGen (getVector n) state
  getVector n = vectorOf n (arbitrary :: Gen Int)
  state = { newSeed: 0, size: 100 }
