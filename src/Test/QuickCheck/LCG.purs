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
import Data.Int.Bits (shl, (.|.))
import Data.Array (replicate)
import Data.Foldable (product)
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
lcgN = product (replicate 31 2) - 1

-- | Step the linear congruential generator
lcgNext :: Int -> Int
lcgNext n = U.fromJust $ fromNumber $ (toNumber lcgM * toNumber n + toNumber lcgC) % toNumber lcgN

-- | Create a random seed
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed =
  -- Because the increment is 0, we must ensure that any seed is coprime to the
  -- modulus. Luckily, this is easy: the modulus has just one prime factor, 2,
  -- so this means our seed will be coprime to the modulus iff it is odd.
  -- We ensure oddness by perfoming a bitwise OR with 1.
  randomInt 0 lcgM <#> (.|. 1)
