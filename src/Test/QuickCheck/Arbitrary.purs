module Test.QuickCheck.Arbitrary
  ( class Arbitrary
  , arbitrary
  , class Coarbitrary
  , coarbitrary
  ) where

import Prelude

import Control.Monad.Gen.Class (chooseBool)
import Control.Monad.Gen.Common as MGC

import Data.Char (toCharCode, fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.String (charCodeAt, fromCharArray, split)
import Data.Tuple (Tuple(..))

import Test.QuickCheck.Gen (Gen, elements, listOf, chooseInt, sized, perturbGen, repeatable, arrayOf, uniform)

-- | The `Arbitrary` class represents those types whose values can be
-- | _randomly-generated_.
-- |
-- | `arbitrary` uses the `Gen` monad to express a random generator for
-- | the type `t`. Combinators in the `Test.QuickCheck.Gen`
-- | module can be used to construct random generators.
class Arbitrary t where
  arbitrary :: Gen t

-- | The `Coarbitrary` class represents types which appear on the left of
-- | an `Arbitrary` function arrow.
-- |
-- | To construct an `Arbitrary` instance for the type `a -> b`, we need to
-- | use the input of type `a` to _perturb_ a random generator for `b`. This
-- | is the role of the `coarbitrary` function.
-- |
-- | `Coarbitrary` instances can be written using the `perturbGen` function.
class Coarbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = chooseBool

instance coarbBoolean :: Coarbitrary Boolean where
  coarbitrary true = perturbGen 1.0
  coarbitrary false = perturbGen 2.0

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform

instance coarbNumber :: Coarbitrary Number where
  coarbitrary = perturbGen

instance arbInt :: Arbitrary Int where
  arbitrary = chooseInt (-1000000) 1000000

instance coarbInt :: Coarbitrary Int where
  coarbitrary = perturbGen <<< toNumber

instance arbString :: Arbitrary String where
  arbitrary = fromCharArray <$> arbitrary

instance coarbString :: Coarbitrary String where
  coarbitrary s = coarbitrary $ (charCodeAt zero <$> split (wrap "") s)

instance arbChar :: Arbitrary Char where
  arbitrary = fromCharCode <$> chooseInt 0 65536

instance coarbChar :: Coarbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbUnit :: Arbitrary Unit where
  arbitrary = pure unit

instance coarbUnit :: Coarbitrary Unit where
  coarbitrary _ = perturbGen 1.0

instance arbOrdering :: Arbitrary Ordering where
  arbitrary = elements $ LT :| [EQ, GT]

instance coarbOrdering :: Coarbitrary Ordering where
  coarbitrary LT = perturbGen 1.0
  coarbitrary EQ = perturbGen 2.0
  coarbitrary GT = perturbGen 3.0

instance arbArray :: Arbitrary a => Arbitrary (Array a) where
  arbitrary = arrayOf arbitrary

instance coarbArray :: Coarbitrary a => Coarbitrary (Array a) where
  coarbitrary = foldl (\f x -> f <<< coarbitrary x) id

instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f (xs :: Array a)) gen

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance coarbTuple :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbMaybe :: Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = MGC.genMaybe arbitrary

instance coarbMaybe :: Coarbitrary a => Coarbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1.0
  coarbitrary (Just a) = coarbitrary a

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = MGC.genEither arbitrary arbitrary

instance coarbEither :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbitraryList :: Arbitrary a => Arbitrary (List a) where
  arbitrary = sized \n -> chooseInt zero n >>= flip listOf arbitrary

instance coarbList :: Coarbitrary a => Coarbitrary (List a) where
  coarbitrary = foldl (\f x -> f <<< coarbitrary x) id

instance arbitraryIdentity :: Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance coarbIdentity :: Coarbitrary a => Coarbitrary (Identity a) where
  coarbitrary (Identity a) = coarbitrary a

instance arbitraryLazy :: Arbitrary a => Arbitrary (Lazy a) where
  arbitrary = arbitrary >>= pure <<< defer <<< const

instance coarbLazy :: Coarbitrary a => Coarbitrary (Lazy a) where
  coarbitrary a = coarbitrary (force a)

instance arbNonEmpty :: (Arbitrary (f a), Arbitrary a) => Arbitrary (NonEmpty f a) where
  arbitrary = NonEmpty <$> arbitrary <*> arbitrary

instance coarbNonEmpty :: (Coarbitrary (f a), Coarbitrary a) => Coarbitrary (NonEmpty f a) where
  coarbitrary (NonEmpty head tail) = coarbitrary head >>> coarbitrary tail

instance arbNonEmptyList :: Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = NonEmptyList <$> arbitrary

instance coarbNonEmptyList :: Coarbitrary a => Coarbitrary (NonEmptyList a) where
  coarbitrary (NonEmptyList nel) = coarbitrary nel
