module Test.QuickCheck.Arbitrary
  ( Arbitrary, arbitrary
  , Coarbitrary, coarbitrary
  ) where

import Data.Function (Fn2(), runFn2)
import Test.QuickCheck.Gen (Gen(), uniform, perturbGen, repeatable)
import Math (round)

class Arbitrary t where
  arbitrary :: Gen t

class Coarbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n * 2) < 1

instance coarbBoolean :: Coarbitrary Boolean where
  coarbitrary true = perturbGen 1
  coarbitrary false = perturbGen 2

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform

instance coarbNumber :: Coarbitrary Number where
  coarbitrary = perturbGen

instance arbString :: Arbitrary String where
  arbitrary = do
    arrNum <- arbitrary
    return $ fromCharArray $ runFn2 map (fromCharCode <<< (* 65535)) arrNum

instance coarbString :: Coarbitrary String where
  coarbitrary s = coarbitrary (runFn2 map charCode $ split s)

instance arbArray :: (Arbitrary a) => Arbitrary [a] where
  arbitrary = do
    b <- arbitrary
    if b then return [] else do
      a <- arbitrary
      as <- arbitrary
      return (a : as)

instance coarbArray :: (Coarbitrary a) => Coarbitrary [a] where
  coarbitrary [] = id
  coarbitrary (x : xs) = coarbitrary xs <<< coarbitrary x

instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (runFn2 map f xs) gen

newtype AlphaNumString = AlphaNumString String

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ AlphaNumString $ fromCharArray $ runFn2 map lookup arrNum
      where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      lookup x = runFn2 charAt index chars
        where
        index = round $ x * (length chars - 1)

instance coarbAlphaNumString :: Coarbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

foreign import length
  """
  function length(s) {
    return s.length;
  }
  """ :: String -> Number

foreign import charAt
  """
  function charAt(i, s) {
    return s.charAt(i);
  }
  """ :: Fn2 Number String String

foreign import fromCharArray
  """
  function fromCharArray(a) {
    return a.join("");
  }
  """ :: [String] -> String

foreign import fromCharCode
  """
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }
  """ :: Number -> String

foreign import charCode
  """
  function charCode(s) {
    return s.charCodeAt(0);
  }
  """ :: String -> Number

foreign import split
  """
  function split(s) {
    return s.split("");
  }
  """ :: String -> [String]

foreign import map
  """
  function map(f, xs) {
    return xs.map(f);
  }
  """ :: forall a b. Fn2 (a -> b) [a] [b]
