module Test.QuickCheck.LCG
  ( Seed()
  , lcgM
  , lcgC
  , lcgN
  , lcgNext
  , randomSeed
  ) where

import Prelude

import Math ((%))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (RANDOM(), randomInt)
import Data.Int (fromNumber, toNumber)
import Data.Int.Bits (shl)
import qualified Data.Maybe.Unsafe as U

type Seed = Int

-- | The *multiplier*: a magic constant for the linear congruential generator
lcgM :: Int
lcgM = 48271

-- | The *increment*: a magic constant for the linear congruential generator
lcgC :: Int
lcgC = 0

-- | The *modulus*: a magic constant for the linear congruential generator
lcgN :: Int
lcgN = 1 `shl` 31 - 1

-- | Step the linear congruential generator
lcgNext :: Int -> Int
lcgNext n = U.fromJust $ fromNumber $ (toNumber lcgM * n' + toNumber lcgC) % toNumber lcgN
  where
  -- Ensure that the input is between seedMin and seedMax; the LCG will not
  -- work well for other inputs.
  n' = ensureBetween (toNumber seedMin) (toNumber seedMax) (toNumber n)

ensureBetween :: Number -> Number -> Number -> Number
ensureBetween min max n =
  let rangeSize = max - min
  in (((n % rangeSize) + rangeSize) % rangeSize) + min

-- | Create a random seed
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed = randomInt seedMin seedMax

-- | The minimum permissible Seed value.
seedMin :: Seed
seedMin = 1

-- | The maximum permissible Seed value.
seedMax :: Seed
seedMax = lcgM - 1
