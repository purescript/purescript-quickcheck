module Test.QuickCheck.Data.ApproxNumber where

import Test.QuickCheck.Arbitrary

-- | A newtype for `Number` whose `Eq` instance uses an epsilon value to allow
-- | for precision erros when comparing.
newtype ApproxNumber = ApproxNumber Number

-- Approximate equality comparison
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

instance arbitraryApproxNumber :: Arbitrary ApproxNumber where
  arbitrary = ApproxNumber <$> arbitrary

instance coarbitraryApproxNumber :: Coarbitrary ApproxNumber where
  coarbitrary (ApproxNumber n) = coarbitrary n

instance eqApproxNumber :: Eq ApproxNumber where
  (==) (ApproxNumber x) (ApproxNumber y) = x =~= y
  (/=) (ApproxNumber x) (ApproxNumber y) = not (x =~= y)

instance ordApproxNumber :: Ord ApproxNumber where
  compare (ApproxNumber x) (ApproxNumber y) = compare x y

instance semiringApproxNumber :: Semiring ApproxNumber where
  (+) (ApproxNumber x) (ApproxNumber y) = ApproxNumber (x + y)
  zero = ApproxNumber zero
  (*) (ApproxNumber x) (ApproxNumber y) = ApproxNumber (x * y)
  one = ApproxNumber one

instance moduloSemiringApproxNumber :: ModuloSemiring ApproxNumber where
  (/) (ApproxNumber x) (ApproxNumber y) = ApproxNumber (x / y)
  mod (ApproxNumber x) (ApproxNumber y) = ApproxNumber (x `mod` y)

instance ringApproxNumber :: Ring ApproxNumber where
  (-) (ApproxNumber x) (ApproxNumber y) = ApproxNumber (x - y)

instance divisionRingApproxNumber :: DivisionRing ApproxNumber
instance numApproxNumber :: Num ApproxNumber
