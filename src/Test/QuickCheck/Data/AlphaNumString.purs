module Test.QuickCheck.Data.AlphaNumString where

import Prelude

import Data.String (fromCharArray, toCharArray)

import Test.QuickCheck.Gen (Gen, arrayOf, oneOf)
import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary, coarbitrary, defaultShrink)

-- | A newtype for `String` whose `Arbitrary` instance generated random
-- | alphanumeric strings.
newtype AlphaNumString = AlphaNumString String

runAlphaNumString :: AlphaNumString -> String
runAlphaNumString (AlphaNumString s) = s

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = AlphaNumString <<< fromCharArray <$> arrayOf anyChar
    where
    rest :: Array Char
    rest = toCharArray "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    anyChar :: Gen Char
    anyChar = oneOf (pure 'a') (map pure rest)
  shrink = defaultShrink

instance coarbAlphaNumString :: Coarbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s
