-- | This module defines the random generator monad used by the `Test.QuickCheck`
-- | module, as well as helper functions for constructing random generators.
module Test.QuickCheck.Gen
  ( Gen()
  , GenState()
  , GenOut()
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
import Data.Array ((!!), length, range)
import Data.Foldable (fold)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..), runAdditive)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (List(..))
import Test.QuickCheck.LCG
import Control.Monad.Trampoline
import qualified Math as M

-- | Tests are parameterized by the `Size` of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size = Int

-- | The state of the random generator monad
type GenState = { newSeed :: Seed, size :: Size }

-- | The output of the random generator monad
type GenOut a = { state :: GenState, value :: a }

-- | The random generator monad
-- |
-- | `Gen` is a state monad which encodes a linear congruential generator.
data Gen a = Gen (GenState -> Trampoline (GenOut a))

unGen :: forall a. Gen a -> GenState -> Trampoline (GenOut a)
unGen (Gen f) = f

-- | Create a random generator for a function type.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ \s -> done { value: \a -> (runGen (f a) s).value, state: s }

-- | Create a random generator which uses the generator state explicitly.
stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = Gen (\s -> unGen (f s) s)

-- | Modify a random generator by setting a new random seed.
variant :: forall a. Seed -> Gen a -> Gen a
variant n g = Gen $ \s -> unGen g s { newSeed = n }

-- | Create a random generator which depends on the size parameter.
sized :: forall a. (Size -> Gen a) -> Gen a
sized f = stateful (\s -> f s.size)

-- | Modify a random generator by setting a new size parameter.
resize :: forall a. Size -> Gen a -> Gen a
resize sz g = Gen $ \s -> unGen g s { size = sz }

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

-- | Create a random generator which generates a vector of random values of a specified size.
vectorOf :: forall a. Int -> Gen a -> Gen (Array a)
vectorOf k g
  | k <= 0    = return []
  | otherwise = sequence $ const g <$> range one k

-- | Create a random generator which selects a value from a non-empty collection with
-- | uniform probability.
elements :: forall a. a -> Array a -> Gen a
elements x xs = do
  n <- chooseInt zero (length xs)
  pure if n == zero then x else fromMaybe x (xs !! (n - one))

-- | Run a random generator
runGen :: forall a. Gen a -> GenState -> GenOut a
runGen gen = runTrampoline <<< unGen gen

-- | Run a random generator, keeping only the randomly-generated result
evalGen :: forall a. Gen a -> GenState -> a
evalGen gen st = (runGen gen st).value

-- | Sample a random generator
sample :: forall r a. Seed -> Size -> Gen a -> Array a
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
lcgStep = Gen f where
  f s = done { value: runSeed s.newSeed, state: s { newSeed = lcgNext s.newSeed } }

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: Gen Number
uniform = (\n -> toNumber n / toNumber lcgN) <$> lcgStep

foreign import float32ToInt32 :: Number -> Int

-- | Perturb a random generator by modifying the current seed
perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n (Gen f) = Gen $ \s -> f (s { newSeed = perturb s.newSeed })
  where
  perturb oldSeed = mkSeed (runSeed (lcgNext (mkSeed (float32ToInt32 n))) + runSeed oldSeed)

instance functorGen :: Functor Gen where
  map f (Gen g) = Gen $ \s -> do
    { value = value, state = state } <- g s
    return { value: f value, state: state }

instance applyGen :: Apply Gen where
  apply = ap

instance applicativeGen :: Applicative Gen where
  pure a = Gen (\s -> done { value: a, state: s })

instance bindGen :: Bind Gen where
  bind (Gen f) g = Gen $ \s -> do
    { value = value, state = state } <- f s
    suspend $ unGen (g value) state

instance monadGen :: Monad Gen
