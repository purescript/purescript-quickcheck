module Test.QuickCheck.LCG where

import Control.Monad.Eff
import Control.Monad.Eff.Random

type LCG = Number

data Gen a = Gen (LCG -> { value :: a, newSeed :: LCG })

runGen :: forall a. Gen a -> LCG -> { value :: a, newSeed :: LCG }
runGen (Gen f) = f

foreign import randomSeed
  "function randomSeed() {\
  \  return Math.floor(Math.random() * (1 << 31));\
  \}" :: forall eff. Eff (random :: Random | eff) Number

runGenWithRandomSeed :: forall a eff. Gen a -> Eff (random :: Random | eff) a
runGenWithRandomSeed g = do
  seed <- randomSeed
  return (runGen g seed).value

stepLCG :: Gen Number
stepLCG = Gen (\l -> { value: l, newSeed: (1664525 * l + 1013904223) % (1 `shl` 31) })

uniform :: Gen Number
uniform = (\n -> n / (1 `shl` 31)) <$> stepLCG

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

