module Test.QuickCheck.LCG
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
    listOf,
    listOf1,
    vectorOf,
    elements,
    runGen,
    evalGen,
    perturbGen,
    uniform
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Traversable
import Data.Monoid.Sum
import qualified Data.Array as A
import qualified Math as M

type LCG = Number
type Size = Number

type GenState = { newSeed :: LCG, size :: Size }

type GenOut a = { state :: GenState, value :: a }

data Gen a = Gen (GenState -> GenOut a)

repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ \s -> { value: \a -> (runGen (f a) s).value, state: s }

stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = Gen (\s -> runGen (f s) s)

variant :: forall a. LCG -> Gen a -> Gen a
variant n g = Gen $ \s -> runGen g s { newSeed = n }

sized :: forall a. (Size -> Gen a) -> Gen a
sized f = stateful (\s -> f s.size)

resize :: forall a. Size -> Gen a -> Gen a
resize sz g = Gen $ \s -> runGen g s { size = sz }

choose :: Number -> Number -> Gen Number
choose a b = (*) (max - min) >>> (+) min <$> uniform where
  min = M.min a b
  max = M.max a b

chooseInt :: Number -> Number -> Gen Number
chooseInt a b = M.floor <$> choose (M.ceil a) (M.floor b + 0.999999999)

oneOf :: forall a. Gen a -> [Gen a] -> Gen a
oneOf x xs = do
  n <- chooseInt 0 (A.length xs)
  if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

frequency :: forall a. Tuple Number (Gen a) -> [Tuple Number (Gen a)] -> Gen a
frequency x xs = let
    xxs   = x : xs
    total = runSum $ fold (((Sum <<< fst) <$> xxs) :: [Sum])
    pick n d [] = d
    pick n d ((Tuple k x) : xs) = if n <= k then x else pick (n - k) d xs
  in do
    n <- chooseInt 1 total
    pick n (snd x) xxs

listOf :: forall a. Gen a -> Gen [a]
listOf g = sized $ \n ->
  do k <- chooseInt 0 n
     vectorOf k g

listOf1 :: forall a. Gen a -> Gen (Tuple a [a])
listOf1 g = sized $ \n ->
  do k  <- chooseInt 0 n
     x  <- g
     xs <- vectorOf (k - 1) g
     return $ Tuple x xs

vectorOf :: forall a. Number -> Gen a -> Gen [a]
vectorOf k g = sequence $ const g <$> (A.range 1 k)

elements :: forall a. a -> [a] -> Gen a
elements x xs = do
  n <- chooseInt 0 (A.length xs)
  pure if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

runGen :: forall a. Gen a -> GenState -> GenOut a
runGen (Gen f) = f

evalGen :: forall a. Gen a -> GenState -> a 
evalGen gen st = (runGen gen st).value
--
-- Magic Numbers
--

lcgM :: Number
lcgM = 1103515245 

lcgC :: Number
lcgC = 12345

lcgN :: Number
lcgN = 1 `shl` 30

lcgNext :: Number -> Number
lcgNext n = (lcgM * n + lcgC) % lcgN

lcgStep :: Gen Number
lcgStep = Gen f where
  f s = { value: s.newSeed, state: s { newSeed = lcgNext s.newSeed } }

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