module Test.QuickCheck.Arbitrary where

import Data.Array (map)
import Data.Char (Char(), toCharCode, fromCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (charCodeAt, fromCharArray, split)
import Data.Tuple (Tuple(..))
import Data.Int (Int(), fromNumber, toNumber)
import Test.QuickCheck.Gen

-- | The `Arbitrary` class represents those types whose values can be
-- | _randomly-generated_.
-- |
-- | `arbitrary` uses the `Gen` monad to express a random generator for
-- | the type `t`. Combinators in the `Test.QuickCheck.Gen`
-- | module can be used to construct random generators.
class Arbitrary t where
  arbitrary :: Gen t

-- | The `CoArbitrary` class represents types which appear on the left of
-- | an `Arbitrary` function arrow.
-- |
-- | To construct an `Arbitrary` instance for the type `a -> b`, we need to
-- | use the input of type `a` to _perturb_ a random generator for `b`. This
-- | is the role of the `coarbitrary` function.
-- |
-- | `CoArbitrary` instances can be written using the `perturbGen` function.
class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n * 2) < 1

instance coarbBoolean :: CoArbitrary Boolean where
  coarbitrary true = perturbGen 1
  coarbitrary false = perturbGen 2

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform

instance coarbNumber :: CoArbitrary Number where
  coarbitrary = perturbGen

instance arbInt :: Arbitrary Int where
  arbitrary = chooseInt (fromNumber (-1000000)) (fromNumber 1000000)

instance coarbInt :: CoArbitrary Int where
  coarbitrary = perturbGen <<< toNumber

instance arbString :: Arbitrary String where
  arbitrary = fromCharArray <$> arbitrary

instance coarbString :: CoArbitrary String where
  coarbitrary s = coarbitrary $ (charCodeAt zero <$> split "" s)

instance arbChar :: Arbitrary Char where
  arbitrary = fromCharCode <<< fromNumber <<< (* 65535) <$> uniform

instance coarbChar :: CoArbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbUnit :: Arbitrary Unit where
  arbitrary = return unit

instance coarbUnit :: CoArbitrary Unit where
  coarbitrary _ = perturbGen 1

instance arbOrdering :: Arbitrary Ordering where
  arbitrary = do
    n <- chooseInt (fromNumber 1) (fromNumber 3)
    return $ case toNumber n of
      1 -> LT
      2 -> EQ
      3 -> GT

instance coarbOrdering :: CoArbitrary Ordering where
  coarbitrary LT = perturbGen 1
  coarbitrary EQ = perturbGen 2
  coarbitrary GT = perturbGen 3

instance arbArray :: (Arbitrary a) => Arbitrary [a] where
  arbitrary = do
    b <- arbitrary
    if b then return [] else do
      a <- arbitrary
      as <- arbitrary
      return (a : as)

instance coarbArray :: (CoArbitrary a) => CoArbitrary [a] where
  coarbitrary [] = id
  coarbitrary (x : xs) = coarbitrary xs <<< coarbitrary x

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f xs) gen

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1
  coarbitrary (Just a) = coarbitrary a

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b
