module Test.QuickCheck.Arbitrary where

import Prelude

import Data.Char (toCharCode, fromCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (charCodeAt, fromCharArray, split)
import Data.Tuple (Tuple(..))
import Data.Int (fromNumber, toNumber)
import Data.Foldable (foldl)
import Test.QuickCheck.Gen

-- | The `Arbitrary` class represents those types whose values can be
-- | _randomly-generated_.
-- |
-- | `arbitrary` uses the `Gen` monad to express a random generator for
-- | the type `t`. Combinators in the `Test.QuickCheck.Gen`
-- | module can be used to construct random generators.
class Arbitrary t where
  arbitrary :: Gen t

class Arbitrary1 f where
  arbitrary1 :: forall a. (Arbitrary a) => Gen (f a)

instance arbArbitrary1 :: (Arbitrary1 f, Arbitrary a) => Arbitrary (f a) where
  arbitrary = arbitrary1

class Arbitrary2 f where
  arbitrary2 :: forall a b. (Arbitrary a, Arbitrary b) => Gen (f a b)

instance arb1Arbitrary2 :: (Arbitrary2 f, Arbitrary a) => Arbitrary1 (f a) where
  arbitrary1 = arbitrary2

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

class Coarbitrary1 f where
  coarbitrary1 :: forall a r. (Coarbitrary a) => f a -> Gen r -> Gen r

instance coarbCoarbitrary1 :: (Coarbitrary1 f, Coarbitrary a) => Coarbitrary (f a) where
  coarbitrary = coarbitrary1

class Coarbitrary2 f where
  coarbitrary2 :: forall a b r. (Coarbitrary a, Coarbitrary b) => f a b -> Gen r -> Gen r

instance coarb1Coarbitrary2 :: (Coarbitrary2 f, Coarbitrary a) => Coarbitrary1 (f a) where
  coarbitrary1 = coarbitrary2

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n * 2.0) < 1.0

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
  coarbitrary s = coarbitrary $ (charCodeAt zero <$> split "" s)

instance arbChar :: Arbitrary Char where
  arbitrary = fromCharCode <$> chooseInt 0 65536

instance coarbChar :: Coarbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbUnit :: Arbitrary Unit where
  arbitrary = return unit

instance coarbUnit :: Coarbitrary Unit where
  coarbitrary _ = perturbGen 1.0

instance arbOrdering :: Arbitrary Ordering where
  arbitrary = oneOf (pure LT) [pure EQ, pure GT]

instance coarbOrdering :: Coarbitrary Ordering where
  coarbitrary LT = perturbGen 1.0
  coarbitrary EQ = perturbGen 2.0
  coarbitrary GT = perturbGen 3.0

instance arb1Array :: Arbitrary1 Array where
  arbitrary1 = arrayOf arbitrary

instance coarb1Array :: Coarbitrary1 Array where
  coarbitrary1 = foldl (\f x -> f <<< coarbitrary x) id

instance arb1Function :: (Coarbitrary a) => Arbitrary1 ((->) a) where
  arbitrary1 = repeatable (\a -> coarbitrary a arbitrary)

instance coarb1Function :: (Arbitrary a) => Coarbitrary1 ((->) a) where
  coarbitrary1 f gen = do
    xs <- arbitrary
    coarbitrary (map f (xs :: Array a)) gen

instance arb2Tuple :: Arbitrary2 Tuple where
  arbitrary2 = Tuple <$> arbitrary <*> arbitrary

instance coarb2Tuple :: Coarbitrary2 Tuple where
  coarbitrary2 (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arb1Maybe :: Arbitrary1 Maybe where
  arbitrary1 = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance coarb1Maybe :: Coarbitrary1 Maybe where
  coarbitrary1 Nothing = perturbGen 1.0
  coarbitrary1 (Just a) = coarbitrary a

instance arb2Either :: Arbitrary2 Either where
  arbitrary2 = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

instance coarb2Either :: Coarbitrary2 Either where
  coarbitrary2 (Left a) = coarbitrary a
  coarbitrary2 (Right b) = coarbitrary b
