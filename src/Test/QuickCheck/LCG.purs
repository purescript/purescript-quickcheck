module Test.QuickCheck.LCG
  ( Seed
  , mkSeed
  , runSeed
  , lcgM
  , lcgC
  , lcgN
  , lcgNext
  , lcgPerturb
  , randomSeed
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)

import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromJust)

import Math ((%))

import Partial.Unsafe (unsafePartial)

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

-- | Perturb a seed value
lcgPerturb :: Number -> Seed -> Seed
lcgPerturb d = Seed <<< go <<< runSeed
  where
  go n = unsafePartial $ fromJust $
    fromNumber $ (toNumber lcgM * toNumber n + d) % toNumber lcgN

-- | Step the linear congruential generator
lcgNext :: Seed -> Seed
lcgNext = lcgPerturb (toNumber lcgC)

-- | Create a random seed
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed = mkSeed <$> randomInt seedMin seedMax

-- | The minimum permissible Seed value.
seedMin :: Int
seedMin = 1

-- | The maximum permissible Seed value.
seedMax :: Int
seedMax = lcgN - 1

-- | A seed for the linear congruential generator. We omit a `Semiring`
-- | instance because there is no `zero` value, as 0 is not an acceptable
-- | seed for the generator.
newtype Seed = Seed Int

mkSeed :: Int -> Seed
mkSeed x = Seed (ensureBetween seedMin seedMax x)

runSeed :: Seed -> Int
runSeed (Seed x) = x

ensureBetween :: Int -> Int -> Int -> Int
ensureBetween min max n =
  let
    rangeSize = max - min
    n' = n `mod` rangeSize
  in
    if n' < min then n' + max else n'

instance showSeed :: Show Seed where
  show (Seed x) = "Seed " <> show x

instance eqSeed :: Eq Seed where
  eq (Seed x) (Seed y) = eq x y
