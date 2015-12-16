-- | This module defines the random generator monad used by the `Test.QuickCheck`
-- | module, as well as helper functions for constructing random generators.
module Test.QuickCheck.Gen
  ( Gen()
  , GenState()
  , Size()
  , repeatable
  , stateful
  , variant
  , sized
  , resize
  , choose
  , chooseInt
  , oneOf
  , frequency
  , arrayOf
  , arrayOf1
  , listOf
  , vectorOf
  , elements
  , runGen
  , evalGen
  , perturbGen
  , uniform
  , sample
  , randomSample
  , randomSample'
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.State (State(), runState, evalState)
import Control.Monad.State.Class (state, modify)
import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Math ((%))
import Data.Array ((!!), length)
import Data.Tuple (Tuple(..))
import Data.Foldable (fold)
import Data.Int (toNumber, fromNumber)
import Data.Maybe (fromMaybe)
import Data.Maybe.Unsafe as U
import Data.Monoid.Additive (Additive(..), runAdditive)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..))
import Data.List (List(..), fromList)
import Test.QuickCheck.LCG
import qualified Math as M

-- | Tests are parameterized by the `Size` of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size = Int

-- | The state of the random generator monad
type GenState = { newSeed :: Seed, size :: Size }

-- | The random generator monad
-- |
-- | `Gen` is a state monad which encodes a linear congruential generator.
type Gen a = State GenState a

-- | Create a random generator for a function type.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = state $ \s -> Tuple (\a -> fst (runGen (f a) s)) (s { newSeed = lcgNext s.newSeed })

-- | Create a random generator which uses the generator state explicitly.
stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = state $ \s -> runGen (f s) s

-- | Modify a random generator by setting a new random seed.
variant :: forall a. Seed -> Gen a -> Gen a
variant n g = state $ \s -> runGen g s { newSeed = n }

-- | Create a random generator which depends on the size parameter.
sized :: forall a. (Size -> Gen a) -> Gen a
sized f = stateful (\s -> f s.size)

-- | Modify a random generator by setting a new size parameter.
resize :: forall a. Size -> Gen a -> Gen a
resize sz g = state $ \s -> runGen g s { size = sz }

-- | Create a random generator which samples a range of `Number`s i
-- | with uniform probability.
choose :: Number -> Number -> Gen Number
choose a b = (*) (max - min) >>> (+) min <$> uniform where
  min = M.min a b
  max = M.max a b

-- | Create a random generator which chooses uniformly distributed
-- | integers from the closed interval `[a, b]`.
chooseInt :: Int -> Int -> Gen Int
chooseInt a b = clamp <$> lcgStep
  where
  clamp :: Int -> Int
  clamp x = case x `mod` (b - a + one) of
              r | r >= 0 -> a + r
                | otherwise -> b + r + one

-- | Create a random generator which selects and executes a random generator from
-- | a non-empty collection of random generators with uniform probability.
oneOf :: forall a. Gen a -> Array (Gen a) -> Gen a
oneOf x xs = do
  n <- chooseInt zero (length xs)
  if n < one then x else fromMaybe x (xs !! (n - one))

-- | Create a random generator which selects and executes a random generator from
-- | a non-empty, weighted collection of random generators.
frequency :: forall a. Tuple Number (Gen a) -> List (Tuple Number (Gen a)) -> Gen a
frequency x xs = let
    xxs   = Cons x xs
    total = runAdditive $ fold (map (Additive <<< fst) xxs :: List (Additive Number))
    pick n d Nil = d
    pick n d (Cons (Tuple k x) xs) = if n <= k then x else pick (n - k) d xs
  in do
    n <- choose zero total
    pick n (snd x) xxs

-- | Create a random generator which generates an array of random values.
arrayOf :: forall a. Gen a -> Gen (Array a)
arrayOf g = sized $ \n ->
  do k <- chooseInt zero n
     vectorOf k g

-- | Create a random generator which generates a non-empty array of random values.
arrayOf1 :: forall a. Gen a -> Gen (Tuple a (Array a))
arrayOf1 g = sized $ \n ->
  do k <- chooseInt zero n
     x <- g
     xs <- vectorOf (k - one) g
     return $ Tuple x xs

replicateMRec :: forall m a. (MonadRec m) => Int -> m a -> m (List a)
replicateMRec k _ | k <= 0 = return Nil
replicateMRec k gen = tailRecM go (Tuple Nil k)
  where
  go :: (Tuple (List a) Int) -> m (Either (Tuple (List a) Int) (List a))
  go (Tuple acc 0) = return $ Right acc
  go (Tuple acc n) = gen <#> \x -> Left (Tuple (Cons x acc) (n - 1))

-- | Create a random generator which generates a list of random values of the specified size.
listOf :: forall a. Int -> Gen a -> Gen (List a)
listOf = replicateMRec

-- | Create a random generator which generates a vector of random values of a specified size.
vectorOf :: forall a. Int -> Gen a -> Gen (Array a)
vectorOf k g = fromList <$> listOf k g

-- | Create a random generator which selects a value from a non-empty collection with
-- | uniform probability.
elements :: forall a. a -> Array a -> Gen a
elements x xs = do
  n <- chooseInt zero (length xs)
  pure if n == zero then x else fromMaybe x (xs !! (n - one))

-- | Run a random generator
runGen :: forall a. Gen a -> GenState -> Tuple a GenState
runGen = runState

-- | Run a random generator, keeping only the randomly-generated result
evalGen :: forall a. Gen a -> GenState -> a
evalGen = evalState

-- | Sample a random generator
sample :: forall a. Seed -> Size -> Gen a -> Array a
sample seed sz g = evalGen (vectorOf sz g) { newSeed: seed, size: sz }

-- | Sample a random generator, using a randomly generated seed
randomSample' :: forall r a. Size -> Gen a -> Eff (random :: RANDOM | r) (Array a)
randomSample' n g = do
  seed <- randomSeed
  return $ sample seed n g

-- | Get a random sample of 10 values
randomSample :: forall r a. Gen a -> Eff (random :: RANDOM | r) (Array a)
randomSample = randomSample' 10

-- | A random generator which simply outputs the current seed
lcgStep :: Gen Int
lcgStep = state f where
  f s = Tuple (runSeed s.newSeed) (s { newSeed = lcgNext s.newSeed })

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: Gen Number
uniform = (\n -> toNumber n / toNumber lcgN) <$> lcgStep

foreign import float32ToInt32 :: Number -> Int

-- | Perturb a random generator by modifying the current seed
perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n gen = do
  modify \s -> s { newSeed = lcgPerturb (toNumber (float32ToInt32 n)) s.newSeed }
  gen
