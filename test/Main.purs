
module Test.Main where

import Prelude

import Control.Bind
import Control.Monad.Eff.Console

import Data.Array (head)
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable
import Data.Traversable
import Data.Generic

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Generic

data Tree a = Leaf | Branch { value :: a, kids :: Array (Tree a) }

-- A test function
anywhere :: forall a. (a -> Boolean) -> Tree a -> Boolean
anywhere _ Leaf = false
anywhere p (Branch o) = p o.value || any (anywhere p) o.kids

derive instance genericTree :: (Generic a) => Generic (Tree a)

instance showTree :: (Generic a) => Show (Tree a) where
  show = gShow

instance eqTree :: (Generic a) => Eq (Tree a) where
  eq = gEq
  
instance arbitraryTree :: (Generic a) => Arbitrary (Tree a) where
  arbitrary = resize 3 gArbitrary -- Avoid very deep trees

instance coarbitraryTree :: (Generic a) => Coarbitrary (Tree a) where
  coarbitrary = gCoarbitrary

main = do
  log "Try with some little Gens first"
  print =<< go 10
  print =<< go 100
  print =<< go 1000
  print =<< go 10000

  log "Testing stack safety of Gen"
  print =<< go 20000
  print =<< go 100000

  log "Generating some arbitrary trees"
  randomSample' 10 (arbitrary :: Gen (Tree Int)) >>= traverse print
  
  log "Eliminating some coarbitrary tree functions"
  quickCheck $ \f g t -> anywhere f t || anywhere g t == anywhere (\a -> f a || g a) (t :: Tree Int)

  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (arbitrary :: Gen Int))
  unsafeHead = fromJust <<< head
