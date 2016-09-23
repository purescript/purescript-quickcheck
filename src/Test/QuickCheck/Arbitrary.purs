module Test.QuickCheck.Arbitrary where

import Prelude
import Data.Char (toCharCode, fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:), fromFoldable, singleton, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray, charCodeAt, fromCharArray, split)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Test.QuickCheck.Gen (Gen, listOf, chooseInt, sized, perturbGen, repeatable, arrayOf, oneOf, uniform)

-- | The `Arbitrary` class represents those types whose values can be
-- | _randomly-generated_.
-- |
-- | `arbitrary` uses the `Gen` monad to express a random generator for
-- | the type `t`. Combinators in the `Test.QuickCheck.Gen`
-- | module can be used to construct random generators.
class Arbitrary t where
  arbitrary :: Gen t
  shrink :: t -> List t

-- | The default shrinking strategy, with no smaller elements.
defaultShrink :: forall a. a -> List a
defaultShrink _ = none

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
  arbitrary = do
    n <- uniform
    pure $ (n * 2.0) < 1.0
  shrink true = singleton false
  shrink false = none

instance coarbBoolean :: Coarbitrary Boolean where
  coarbitrary true = perturbGen 1.0
  coarbitrary false = perturbGen 2.0

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform
  shrink n = fromFoldable [n / 2.0, n - 1.0, -1.0, 1.0, 0.0]

instance coarbNumber :: Coarbitrary Number where
  coarbitrary = perturbGen

instance arbInt :: Arbitrary Int where
  arbitrary = chooseInt (-1000000) 1000000
  shrink 0 = none
  shrink 1 = singleton 0
  shrink (-1) = singleton 0
  shrink n = fromFoldable [n / 2, n - 1, -1, 1, 0]

instance coarbInt :: Coarbitrary Int where
  coarbitrary = perturbGen <<< toNumber

instance arbString :: Arbitrary String where
  arbitrary = fromCharArray <$> arbitrary
  shrink = map fromCharArray <<< shrink <<< toCharArray

instance coarbString :: Coarbitrary String where
  coarbitrary s = coarbitrary $ (charCodeAt zero <$> split "" s)

instance arbChar :: Arbitrary Char where
  arbitrary = fromCharCode <$> chooseInt 0 65536
  shrink = map fromCharCode <<< shrink <<< toCharCode

instance coarbChar :: Coarbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbUnit :: Arbitrary Unit where
  arbitrary = pure unit
  shrink = defaultShrink

instance coarbUnit :: Coarbitrary Unit where
  coarbitrary _ = perturbGen 1.0

instance arbOrdering :: Arbitrary Ordering where
  arbitrary = oneOf (pure LT) [pure EQ, pure GT]
  shrink GT = fromFoldable [LT, EQ]
  shrink EQ = fromFoldable [LT]
  shrink LT = none

instance coarbOrdering :: Coarbitrary Ordering where
  coarbitrary LT = perturbGen 1.0
  coarbitrary EQ = perturbGen 2.0
  coarbitrary GT = perturbGen 3.0

instance arbArray :: Arbitrary a => Arbitrary (Array a) where
  arbitrary = arrayOf arbitrary
  shrink = map toUnfoldable <<< shrink <<< fromFoldable

instance coarbArray :: Coarbitrary a => Coarbitrary (Array a) where
  coarbitrary = foldl (\f x -> f <<< coarbitrary x) id

instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)
  shrink = defaultShrink

instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f (xs :: Array a)) gen

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary
  shrink (Tuple a b) =
    map (Tuple a) (shrink b)
      <> map (_ `Tuple` b) (shrink a)

instance coarbTuple :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbMaybe :: Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary
  shrink Nothing = none
  shrink (Just a) = Nothing : map Just (shrink a)

instance coarbMaybe :: Coarbitrary a => Coarbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1.0
  coarbitrary (Just a) = coarbitrary a

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary
  shrink (Left a) = map Left (shrink a)
  shrink (Right b) = map Right (shrink b)

instance coarbEither :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbitraryList :: Arbitrary a => Arbitrary (List a) where
  arbitrary = sized \n -> chooseInt zero n >>= flip listOf arbitrary
  shrink Nil = none
  shrink (x : xs) =
    shrink xs
      <> map (_ : xs) (shrink x)
        <> map (x : _) (shrink xs)

instance coarbList :: Coarbitrary a => Coarbitrary (List a) where
  coarbitrary = foldl (\f x -> f <<< coarbitrary x) id

instance arbitraryIdentity :: Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
  shrink (Identity a) = map Identity (shrink a)

instance coarbIdentity :: Coarbitrary a => Coarbitrary (Identity a) where
  coarbitrary (Identity a) = coarbitrary a

instance arbitraryLazy :: Arbitrary a => Arbitrary (Lazy a) where
  arbitrary = arbitrary >>= pure <<< defer <<< const
  shrink = map pure <<< shrink <<< force

instance coarbLazy :: Coarbitrary a => Coarbitrary (Lazy a) where
  coarbitrary a = coarbitrary (force a)
