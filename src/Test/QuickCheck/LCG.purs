module Test.QuickCheck.LCG
  ( Seed()
  , lcgM
  , lcgC
  , lcgN
  , lcgNext
  , randomSeed
  ) where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (RANDOM(), random)
import Data.Int (Int(), fromNumber, toNumber)
import Data.Int.Bits (shl)

type Seed = Int

-- | A magic constant for the linear congruential generator
lcgM :: Int
lcgM = fromNumber lcgM'

lcgM' :: Number
lcgM' = 1103515245

-- | A magic constant for the linear congruential generator
lcgC :: Int
lcgC = fromNumber 12345

-- | A magic constant for the linear congruential generator
lcgN :: Int
lcgN = one `shl` fromNumber 30

-- | Step the linear congruential generator
lcgNext :: Int -> Int
lcgNext n = (lcgM * n + lcgC) `mod` lcgN

randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed = fromNumber <<< (lcgM' *) <$> random
