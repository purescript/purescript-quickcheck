
module Test.Main where

import Prelude
import Data.Foldable
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad.Eff.Console

main = do
  log "Testing stack safety of Gen"
  print $ sum $ _.value $ runGen gen state
  where
  gen = vectorOf 20000 (arbitrary :: Gen Int)
  state = { newSeed: 0, size: 100 }
