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
randomSeed = randomInt 1 lcgM
