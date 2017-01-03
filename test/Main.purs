module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array.Partial (head)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Partial.Unsafe (unsafePartial)

import Test.QuickCheck.Arbitrary (arbitrary, genericArbitrary, class Arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf, randomSample')

data Foo a = F0 a | F1 a a | F2 { foo :: a, bar :: Array a }
derive instance genericFoo :: Generic (Foo a) _
instance showFoo :: Show a => Show (Foo a) where show = genericShow
instance arbitraryFoo :: Arbitrary a => Arbitrary (Foo a) where arbitrary = genericArbitrary 

main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  log "Try with some little Gens first"
  logShow =<< go 10
  logShow =<< go 100
  logShow =<< go 1000
  logShow =<< go 10000

  log "Testing stack safety of Gen"
  logShow =<< go 20000
  logShow =<< go 100000

  log "Generating via Generic"
  logShow =<< randomSample' 10 (arbitrary :: Gen (Foo Int))

  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (arbitrary :: Gen Int))

  unsafeHead :: forall x. Array x -> x
  unsafeHead xs = unsafePartial (head xs)
