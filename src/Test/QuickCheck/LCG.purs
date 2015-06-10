module Test.QuickCheck.LCG
  ( Seed()
  , lcgM
  , lcgC
  , lcgN
  , lcgNext
  , randomSeed
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (RANDOM(), randomInt)
import Data.Int (fromNumber, toNumber)
import Data.Int.Bits (shl)

type Seed = Int

-- | A magic constant for the linear congruential generator
lcgM :: Int
lcgM = 1103515245

-- | A magic constant for the linear congruential generator
lcgC :: Int
lcgC = 12345

-- | A magic constant for the linear congruential generator
lcgN :: Int
lcgN = one `shl` 30

-- | Step the linear congruential generator
lcgNext :: Int -> Int
lcgNext n = (lcgM * n + lcgC) `mod` lcgN

randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed = randomInt 0 lcgM
