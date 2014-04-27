module Test.QuickCheck.LCG where

import Control.Monad.Eff
import Control.Monad.Eff.Random

type LCG = Number

data Gen a = Gen (LCG -> { value :: a, newSeed :: LCG })

runGen :: forall a. Gen a -> LCG -> { value :: a, newSeed :: LCG }
runGen (Gen f) = f

evalGen :: forall a. Gen a -> LCG -> a 
evalGen gen seed = (runGen gen seed).value

foreign import randomSeed
  "function randomSeed() {\
  \  return Math.floor(Math.random() * (1 << 30));\
  \}" :: forall eff. Eff (random :: Random | eff) Number

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
lcgStep = Gen (\l -> { value: l, newSeed: lcgNext l })

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
perturbGen n (Gen f) = Gen $ \l -> f (lcgNext (float32ToInt32 n) + l)

instance functorGen :: Functor Gen where
  (<$>) f (Gen g) = Gen $ \l -> case g l of
    { value = value, newSeed = newSeed } -> { value: f value, newSeed: newSeed }

instance applyGen :: Apply Gen where
  (<*>) (Gen f) (Gen x) = Gen $ \l ->
    case f l of
      { value = f', newSeed = l' } -> case x l' of
        { value = x', newSeed = l'' } -> { value: f' x', newSeed: l'' }

instance applicativeGen :: Applicative Gen where
  pure a = Gen (\l -> { value: a, newSeed: l })

instance bindGen :: Bind Gen where
  (>>=) (Gen f) g = Gen $ \l -> case f l of
    { value = value, newSeed = newSeed } -> runGen (g value) newSeed

instance monadGen :: Monad Gen

