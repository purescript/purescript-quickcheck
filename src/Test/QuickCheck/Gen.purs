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
  , showSample
  , showSample'
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), print)
import Data.Array ((!!), length, range)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..), runAdditive)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (List(..))
import Test.QuickCheck.LCG
import Control.Monad.Rec.Class
import qualified Math as M

-- | Tests are parameterized by the `Size` of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size = Int

-- | The state of the random generator monad
type GenState = { newSeed :: Seed, size :: Size }

-- | The output of the random generator monad
type GenOut a = { state :: GenState, value :: a }

-- | An internal type, used for stack-safe tail recursion.
type Accum a = { state :: GenState, gen :: Gen a }

-- | The random generator monad
-- |
-- | `Gen` is a state monad which encodes a linear congruential generator.
data Gen a = Gen (GenState -> Either (Accum a) (GenOut a))

unGen :: forall a. Gen a -> GenState -> Either (Accum a) (GenOut a)
unGen (Gen f) = f

-- | Create a random generator for a function type.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ \s -> Right { value: \a -> (runGen (f a) s).value, state: s }

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

-- | Create a random generator which chooses an integer from a range.
chooseInt :: Int -> Int -> Gen Int
chooseInt a b = clamp <$> lcgStep
  where
  clamp :: Int -> Int
  clamp x = case x `mod` (b - a) of
              r | r >= 0 -> a + r
                | otherwise -> b + r

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
vectorOf k g = sequence $ const g <$> range one k

-- | Create a random generator which selects a value from a non-empty collection with
-- | uniform probability.
elements :: forall a. a -> Array a -> Gen a
elements x xs = do
  n <- chooseInt zero (length xs)
  pure if n == zero then x else fromMaybe x (xs !! (n - one))

-- | Run a random generator
runGen :: forall a. Gen a -> GenState -> GenOut a
runGen gen state = tailRec go { gen: gen, state: state }
  where
  go accum = unGen accum.gen accum.state

-- | Run a random generator, keeping only the randomly-generated result
evalGen :: forall a. Gen a -> GenState -> a
evalGen gen st = (runGen gen st).value

-- | Sample a random generator
sample :: forall r a. Size -> Gen a -> Array a
sample sz g = evalGen (vectorOf sz g) { newSeed: zero, size: sz }

-- | Print a random sample to the console
showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (console :: CONSOLE | r) Unit
showSample' n g = print $ sample n g

-- | Print a random sample of 10 values to the console
showSample :: forall r a. (Show a) => Gen a -> Eff (console :: CONSOLE | r) Unit
showSample = showSample' 10

-- | A random generator which simply outputs the current seed
lcgStep :: Gen Int
lcgStep = Gen f where
  f s = Right { value: s.newSeed, state: s { newSeed = lcgNext s.newSeed } }

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: Gen Number
uniform = (\n -> toNumber n / toNumber lcgN) <$> lcgStep

foreign import float32ToInt32 :: Number -> Int

-- | Perturb a random generator by modifying the current seed
perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n (Gen f) = Gen $ \s -> f (s { newSeed = lcgNext (float32ToInt32 n) + s.newSeed })

instance functorGen :: Functor Gen where
  map f gen = gen >>= (return <<< f)

instance applyGen :: Apply Gen where
  apply = ap

instance applicativeGen :: Applicative Gen where
  pure a = Gen (\s -> Right { value: a, state: s })

instance bindGen :: Bind Gen where
  bind (Gen f) g = Gen $ \s ->
    case f s of
      Right out  -> Left { state: out.state,   gen: g out.value     }
      Left accum -> Left { state: accum.state, gen: accum.gen >>= g }

instance monadGen :: Monad Gen
