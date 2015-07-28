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

-- | The *modulus*: a magic constant for the linear congruential generator.
-- | It is equal to 2^31 - 1, a Mersenne prime. It is useful for this value to
-- | be prime, because then the requirement of the initial seed being coprime
-- | to the modulus is satisfied when the seed is between 1 and lcgN - 1.
lcgN :: Int
lcgN = 2147483647

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
