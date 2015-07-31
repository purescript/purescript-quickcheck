
module Test.Main where

import Prelude
import Control.Bind
import Data.Array (head)
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad.Eff.Console

main = do
  log "Try with some little Gens first"
  print =<< go 10
  print =<< go 100
  print =<< go 1000
  print =<< go 10000

  log "Testing stack safety of Gen"
  print =<< go 20000

  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (arbitrary :: Gen Int))
  unsafeHead = fromJust <<< head
