module Test.QuickCheck.Data.AlphaNumString where

import Data.String (fromCharArray, length)
import Data.String.Unsafe (charAt)
import Math (round)
import Test.QuickCheck.Arbitrary

-- | A newtype for `String` whose `Arbitrary` instance generated random
-- | alphanumeric strings.
newtype AlphaNumString = AlphaNumString String

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ AlphaNumString <<< fromCharArray $ lookup <$> arrNum where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      lookup x = charAt index chars where
        index = round $ x * (length chars - 1)

instance coarbAlphaNumString :: CoArbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s
