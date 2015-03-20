-- | This module defines the random generator monad used by the `Test.QuickCheck` 
-- | module, as well as helper functions for constructing random generators.

module Test.QuickCheck.Gen
  (
    Gen(),
    GenState(),
    GenOut(),
    Size(),
    LCG(),
    repeatable,
    stateful,
    variant,
    sized,
    resize,
    choose,
    chooseInt,
    oneOf,
    frequency,
    arrayOf,
    arrayOf1,
    vectorOf,
    elements,
    runGen,
    evalGen,
    perturbGen,
    uniform,
    showSample,
    showSample'
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Debug.Trace
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Traversable
import Data.Monoid.Additive
import qualified Data.Array as A
import qualified Math as M
import Extensions

-- | A seed for the random number generator
type LCG = Number

-- | Tests are parameterized by the `Size` of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size = Number

-- | The state of the random generator monad
type GenState = { newSeed :: LCG, size :: Size }

-- | The output of the random generator monad
type GenOut a = { state :: GenState, value :: a }

-- | The random generator monad
-- |
-- | `Gen` is a state monad which encodes a linear congruential generator.
data Gen a = Gen (GenState -> GenOut a)

-- | Create a random generator for a function type.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ \s -> { value: \a -> (runGen (f a) s).value, state: s }

-- | Create a random generator which uses the generator state explicitly.
stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = Gen (\s -> runGen (f s) s)

-- | Modify a random generator by setting a new random seed.
variant :: forall a. LCG -> Gen a -> Gen a
variant n g = Gen $ \s -> runGen g s { newSeed = n }

-- | Create a random generator which depends on the size parameter.
sized :: forall a. (Size -> Gen a) -> Gen a
sized f = stateful (\s -> f s.size)

-- | Modify a random generator by setting a new size parameter.
resize :: forall a. Size -> Gen a -> Gen a
resize sz g = Gen $ \s -> runGen g s { size = sz }

-- | Create a random generator which samples a range of `Number`s i
-- | with uniform probability.
choose :: Number -> Number -> Gen Number
choose a b = (*) (max - min) >>> (+) min <$> uniform where
  min = M.min a b
  max = M.max a b

-- | Create a random generator which chooses an integer from a range.
chooseInt :: Number -> Number -> Gen Number
chooseInt a b = M.floor <$> choose (M.ceil a) (M.floor b + 0.999999999)

-- | Create a random generator which selects and executes a random generator from
-- | a non-empty collection of random generators with uniform probability.
oneOf :: forall a. Gen a -> [Gen a] -> Gen a
oneOf x xs = do
  n <- chooseInt 0 (A.length xs)
  if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

-- | Create a random generator which selects and executes a random generator from
-- | a non-empty, weighted collection of random generators.
frequency :: forall a. Tuple Number (Gen a) -> [Tuple Number (Gen a)] -> Gen a
frequency x xs = let
    xxs   = x : xs
    total = runAdditive $ fold (((Additive <<< fst) <$> xxs) :: [Additive Number])
    pick n d [] = d
    pick n d ((Tuple k x) : xs) = if n <= k then x else pick (n - k) d xs
  in do
    n <- chooseInt 1 total
    pick n (snd x) xxs

-- | Create a random generator which generates an array of random values.
arrayOf :: forall a. Gen a -> Gen [a]
arrayOf g = sized $ \n -> 
  do k <- chooseInt 0 n
     vectorOf k g

-- | Create a random generator which generates a non-empty array of random values.
arrayOf1 :: forall a. Gen a -> Gen (Tuple a [a])
arrayOf1 g = sized $ \n ->
  do k  <- chooseInt 0 n
     x  <- g
     xs <- vectorOf k g
     return $ Tuple x xs

-- | Create a random generator which generates a vector of random values of a specified size.
vectorOf :: forall a. Number -> Gen a -> Gen [a]
vectorOf k g = if k < 1
                  then return []
                  else sequence $ const g <$> (A.range 1 k)

-- | Create a random generator which selects a value from a non-empty collection with
-- | uniform probability.
elements :: forall a. a -> [a] -> Gen a
elements x xs = do
  n <- chooseInt 0 (A.length xs)
  pure if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

-- | Run a random generator
runGen :: forall a. Gen a -> GenState -> GenOut a
runGen (Gen f) = f

-- | Run a random generator, keeping only the randomly-generated result
evalGen :: forall a. Gen a -> GenState -> a
evalGen gen st = (runGen gen st).value

-- | Sample a random generator
sample :: forall r a. Size -> Gen a -> [a]
sample sz g = evalGen (vectorOf sz g) { newSeed: 0, size: sz }

-- | Print a random sample to the console
showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (trace :: Trace | r) Unit
showSample' n g = print $ sample n g

-- | Print a random sample of 10 values to the console
showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit
showSample = showSample' 10

-- | A magic constant for the linear congruential generator
lcgM :: Number
lcgM = 1103515245

-- | A magic constant for the linear congruential generator
lcgC :: Number
lcgC = 12345

-- | A magic constant for the linear congruential generator
lcgN :: Number
lcgN = 1 `shl` 30

-- | Step the linear congruential generator
lcgNext :: Number -> Number
lcgNext n = (lcgM * n + lcgC) % lcgN

-- | A random generator which simply outputs the current seed
lcgStep :: Gen Number
lcgStep = Gen f where
  f s = { value: s.newSeed, state: s { newSeed = lcgNext s.newSeed } }

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: Gen Number
uniform = (\n -> n / (1 `shl` 30)) <$> lcgStep

foreign import float32ToInt32
  "function float32ToInt32(n) {\
  \  var arr = new ArrayBuffer(4);\
  \  var fv = new Float32Array(arr);\
  \  var iv = new Int32Array(arr);\
  \  fv[0] = n;\
  \  return iv[0];\
  \}" :: Number -> Number

-- | Perturb a random generator by modifying the current seed
perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n (Gen f) = Gen $ \s -> f (s { newSeed = lcgNext (float32ToInt32 n) + s.newSeed })

instance functorGen :: Functor Gen where
  (<$>) f (Gen g) = Gen $ \s -> case g s of
    { value = value, state = state } -> { value: f value, state: state }

instance applyGen :: Apply Gen where
  (<*>) (Gen f) (Gen x) = Gen $ \s ->
    case f s of
      { value = f', state = s' } -> case x s' of
        { value = x', state = s'' } -> { value: f' x', state: s'' }

instance applicativeGen :: Applicative Gen where
  pure a = Gen (\s -> { value: a, state: s })

instance bindGen :: Bind Gen where
  (>>=) (Gen f) g = Gen $ \s -> case f s of
    { value = value, state = state } -> runGen (g value) state

instance monadGen :: Monad Gen
