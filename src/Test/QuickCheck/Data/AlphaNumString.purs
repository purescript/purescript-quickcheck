module Test.QuickCheck.Data.AlphaNumString where

import Data.Int (fromNumber, toNumber)
import Data.String (fromCharArray, length)
import Data.String.Unsafe (charAt)
import Math (round)
import Test.QuickCheck.Arbitrary

-- | A newtype for `String` whose `Arbitrary` instance generated random
-- | alphanumeric strings.
newtype AlphaNumString = AlphaNumString String

runAlphaNumString (AlphaNumString s) = s

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ AlphaNumString <<< fromCharArray $ lookup <$> arrNum
    where
    chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    lookup x = let index = fromNumber $ x * (toNumber (length chars) - 1)
               in charAt index chars


instance coarbAlphaNumString :: Coarbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s
