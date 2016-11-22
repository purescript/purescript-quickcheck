-- | This module defines the random generator monad used by the `Test.QuickCheck`
-- | module, as well as helper functions for constructing random generators.
module Test.QuickCheck.Gen
  ( Gen
  , unGen
  , GenState
  , Size
  , repeatable
  , stateful
  , variant
  , suchThat
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

import Control.Alt (class Alt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (State, runState, evalState)
import Control.Monad.State.Class (state, modify)
import Control.Monad.State.Trans (StateT)

import Data.Array ((!!), length)
import Data.Foldable (fold)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.List (List(..), toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)

import Math as M

import Test.QuickCheck.LCG (Seed, lcgPerturb, lcgN, lcgNext, runSeed, randomSeed)

-- | Tests are parameterized by the `Size` of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size = Int

-- | The state of the random generator monad
type GenState = { newSeed :: Seed, size :: Size }

-- | The random generator monad
-- |
-- | `Gen` is a state monad which encodes a linear congruential generator.
newtype Gen a = Gen (StateT GenState Identity a)

derive newtype instance functorGen :: Functor Gen
derive newtype instance applyGen :: Apply Gen
derive newtype instance applicativeGen :: Applicative Gen
derive newtype instance bindGen :: Bind Gen
derive newtype instance monadGen :: Monad Gen
derive newtype instance altGen :: Alt Gen
derive newtype instance monadRecGen :: MonadRec Gen

-- | Exposes the underlying State implementation.
unGen :: forall a. Gen a -> State GenState a
unGen (Gen st) = st

-- | Create a random generator for a function type.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ state \s -> Tuple (\a -> fst (runGen (f a) s)) (s { newSeed = lcgNext s.newSeed })

-- | Create a random generator which uses the generator state explicitly.
stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = Gen $ state \s -> runGen (f s) s

-- | Modify a random generator by setting a new random seed.
variant :: forall a. Seed -> Gen a -> Gen a
variant n g = Gen $ state \s -> runGen g s { newSeed = n }

-- | Ensure that a generator only produces values that match a predicate. If
-- | the predicate always returns false the generator will loop forever.
suchThat :: forall a. Gen a -> (a -> Boolean) -> Gen a
suchThat gen pred = tailRecM go unit
  where
  go :: Unit -> Gen (Step Unit a)
  go _ = do
    a <- gen
    pure if pred a then Done a else Loop unit

-- | Create a random generator which depends on the size parameter.
sized :: forall a. (Size -> Gen a) -> Gen a
sized f = stateful (\s -> f s.size)

-- | Modify a random generator by setting a new size parameter.
resize :: forall a. Size -> Gen a -> Gen a
resize sz g = Gen $ state \s -> runGen g s { size = sz }

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
    total = unwrap $ fold (map (Additive <<< fst) xxs :: List (Additive Number))
    pick n d Nil = d
    pick n d (Cons (Tuple k x') xs') = if n <= k then x' else pick (n - k) d xs'
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
     pure $ Tuple x xs

replicateMRec :: forall m a. MonadRec m => Int -> m a -> m (List a)
replicateMRec k _ | k <= 0 = pure Nil
replicateMRec k gen = tailRecM go (Tuple Nil k)
  where
  go :: (Tuple (List a) Int) -> m (Step (Tuple (List a) Int) (List a))
  go (Tuple acc 0) = pure $ Done acc
  go (Tuple acc n) = gen <#> \x -> Loop (Tuple (Cons x acc) (n - 1))

-- | Create a random generator which generates a list of random values of the specified size.
listOf :: forall a. Int -> Gen a -> Gen (List a)
listOf = replicateMRec

-- | Create a random generator which generates a vector of random values of a specified size.
vectorOf :: forall a. Int -> Gen a -> Gen (Array a)
vectorOf k g = toUnfoldable <$> listOf k g

-- | Create a random generator which selects a value from a non-empty collection with
-- | uniform probability.
elements :: forall a. a -> Array a -> Gen a
elements x xs = do
  n <- chooseInt zero (length xs)
  pure if n == zero then x else fromMaybe x (xs !! (n - one))

-- | Run a random generator
runGen :: forall a. Gen a -> GenState -> Tuple a GenState
runGen = runState <<< unGen

-- | Run a random generator, keeping only the randomly-generated result
evalGen :: forall a. Gen a -> GenState -> a
evalGen = evalState <<< unGen

-- | Sample a random generator
sample :: forall a. Seed -> Size -> Gen a -> Array a
sample seed sz g = evalGen (vectorOf sz g) { newSeed: seed, size: sz }

-- | Sample a random generator, using a randomly generated seed
randomSample' :: forall r a. Size -> Gen a -> Eff (random :: RANDOM | r) (Array a)
randomSample' n g = do
  seed <- randomSeed
  pure $ sample seed n g

-- | Get a random sample of 10 values
randomSample :: forall r a. Gen a -> Eff (random :: RANDOM | r) (Array a)
randomSample = randomSample' 10

-- | A random generator which simply outputs the current seed
lcgStep :: Gen Int
lcgStep = Gen $ state f where
  f s = Tuple (runSeed s.newSeed) (s { newSeed = lcgNext s.newSeed })

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: Gen Number
uniform = (\n -> toNumber n / toNumber lcgN) <$> lcgStep

foreign import float32ToInt32 :: Number -> Int

-- | Perturb a random generator by modifying the current seed
perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n gen = Gen do
  modify \s -> s { newSeed = lcgPerturb (toNumber (float32ToInt32 n)) s.newSeed }
  unGen gen
