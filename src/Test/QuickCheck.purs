module Test.QuickCheck 
  ( (<?>)
  , AlphaNumString(..)
  , Arbitrary
  , CoArbitrary
  , FairTuple(..)
  , FairEither(..)
  , LastEnum(..)
  , Negative(..)
  , NonZero(..)
  , Positive(..)
  , QC(..)
  , arbitrary
  , assert
  , coarbitrary
  , quickCheck
  , quickCheck'
  , quickCheckPure
  , Result(..)
  , runAlphaNumString
  , runFairEither
  , runFairTuple
  , runLastEnum
  , runNegative
  , runNonZero
  , runPositive
  , runSignum
  , Signum(..)
  , smallCheck
  , smallCheckPure
  , statCheck
  , statCheckPure
  , test
  , Testable
  ) where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Trampoline
import Control.Bind
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Data.Array
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid
import Data.Enum
import Data.Traversable
import Math

import qualified Data.String as S
import Data.Char

import Test.QuickCheck.LCG

class Arbitrary t where
  arbitrary :: Gen t

class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

class Testable prop where
  test :: prop -> Gen Result

newtype AlphaNumString = AlphaNumString String

newtype Positive = Positive Number

newtype Negative = Negative Number

newtype NonZero  = NonZero Number

newtype Signum   = Signum Number

newtype LastEnum a = LastEnum a

newtype FairEither a b = FairEither (Either a b)

newtype FairTuple a b = FairTuple (Tuple a b)

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception | eff) a

data Result = Success | Failed String

(<?>) :: Boolean -> String -> Result
(<?>) true  = const Success
(<?>) false = Failed

quickCheckPure :: forall prop. (Testable prop) => Number -> Seed -> prop -> [Result]
quickCheckPure n s prop = runTrampoline $ sample' n (defState s) (test prop)

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
quickCheck' n prop = check (quickCheckPure n) prop

-- | Checks the proposition for 100 random values.
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' 100 prop

smallCheckPure :: forall prop. (Testable prop) => Number -> prop -> [Result]
smallCheckPure s prop = runTrampoline $ collectAll (defState s) (test prop)

-- | Exhaustively checks the proposition for all possible values. Assumes the
-- | generator is a finite generator.
smallCheck :: forall prop. (Testable prop) => prop -> QC Unit
smallCheck prop = check smallCheckPure prop

statCheckPure :: forall prop. (Testable prop) => Seed -> Number -> prop -> Result
statCheckPure s freq prop = try 100 where
  try x = let measure n = let results = quickCheckPure n s prop
                          in  (countSuccesses results) / (length results)

              measure' 0 = []
              measure' n = measure' (n - 1) <> [measure (n * x)]

              freqs = measure' 4

              dists = (Math.abs <<< (-) freq) <$> freqs 

              dirs  = zipWith (\a b -> a - b) (1 : dists) dists

              fails = length $ filter ((>) 0) dirs

              succs = filter ((<=) 0) dirs

          in  if fails > 1 then 
                if x < 1000000 then try (x * 10)
                else Failed $ "Divergence of statistical test: freqs = " ++ show freqs ++ ", dists = " ++ show dists ++ ", dirs = " ++ show dirs ++ ", fails: " ++ show fails
              else maybe (Failed "Error!") (\l -> if l > 0.5 then Failed $ "Final convergence distance too low: " ++ show l else Success) (last succs)

-- | Checks that the proposition has a certain probability of being true for 
-- | arbitrary values.
statCheck :: forall prop. (Testable prop) => Number -> prop -> QC Unit
statCheck freq prop = do
  seed <- random
  trace <<< show $ statCheckPure seed freq prop

-- | Checks that the specified proposition holds. Useful for unit tests.
assert :: forall prop. (Testable prop) => prop -> QC Unit
assert = quickCheck' 1

defState :: Number -> GenState
defState s = (GenState {seed: s, size: 10})

check :: forall prop. (Testable prop) => (Seed -> prop -> [Result]) -> prop -> QC Unit
check f prop = do
  seed <- random
  let results   = f seed prop
  let successes = countSuccesses results
  trace $ show successes ++ "/" ++ show (length results) ++ " test(s) passed."
  throwOnFirstFailure 1 results

throwOnFirstFailure :: Number -> [Result] -> QC Unit
throwOnFirstFailure _ []                  = return unit
throwOnFirstFailure n (Failed msg : _)    = throwException $ error $ "Test " ++ show n ++ " failed: \n" ++ msg
throwOnFirstFailure n (_          : rest) = throwOnFirstFailure (n + 1) rest

countSuccesses :: [Result] -> Number
countSuccesses = countSuccesses' 0 
  where countSuccesses' acc []               = acc
        countSuccesses' acc (Success : rest) = countSuccesses' (acc + 1) rest
        countSuccesses' acc (_       : rest) = countSuccesses' acc rest

maxNumber :: Number
maxNumber = 9007199254740992

runAlphaNumString (AlphaNumString s) = s

runSignum (Signum n) = n

runPositive (Positive n) = n

runNegative (Negative n) = n

runNonZero (NonZero n) = n

runFairEither (FairEither e) = e

runFairTuple (FairTuple t) = t

runLastEnum :: forall a. LastEnum a -> a
runLastEnum (LastEnum a) = a

instance eqResult :: Eq Result where
  (==) Success Success = true
  (==) (Failed m1) (Failed m2) = m1 == m2
  (==) _ _ = false

  (/=) a b = not (a == b)

instance showResult :: Show Result where
  show Success      = "Success"
  show (Failed msg) = "Failed: " ++ msg

instance semigroupResult :: Semigroup Result where
  (<>) Success Success          = Success
  (<>) (Failed msg) Success     = Failed msg
  (<>) Success (Failed msg)     = Failed msg
  (<>) (Failed m1) (Failed m2)  = Failed (m1 ++ "\n" ++ m2)

instance monoidResult :: Monoid Result where
  mempty = Success

instance eqFairTuple :: (Eq a, Eq b) => Eq (FairTuple a b) where
  (==) (FairTuple t1) (FairTuple t2) = t1 == t2

  (/=) t1 t2 = not (t1 == t2)

instance ordFairTuple :: (Ord a, Ord b) => Ord (FairTuple a b) where
  compare (FairTuple t1) (FairTuple t2) = t1 `compare` t2

instance showFairTuple :: (Show a, Show b) => Show (FairTuple a b) where
  show (FairTuple t) = "FairTuple (" ++ show t ++ ")"

instance semigroupFairTuple :: (Semigroup a, Semigroup b) => Semigroup (FairTuple a b) where
  (<>) (FairTuple t1) (FairTuple t2) = FairTuple $ t1 <> t2

instance monoidFairTuple :: (Monoid a, Monoid b) => Monoid (FairTuple a b) where
  mempty = FairTuple $ Tuple mempty mempty

instance enumFairTuple :: (Enum a, Enum b) => Enum (FairTuple a b) where
  cardinality = fairTupleCardinality f where f (Cardinality sz1) (Cardinality sz2) = Cardinality (sz1 * sz2)

  firstEnum = FairTuple firstEnum

  lastEnum = FairTuple lastEnum

  succ (FairTuple e) = FairTuple <$> succ e

  pred (FairTuple e) = FairTuple <$> pred e

instance eqFairEither :: (Eq a, Eq b) => Eq (FairEither a b) where
  (==) (FairEither t1) (FairEither t2) = t1 == t2

  (/=) t1 t2 = not (t1 == t2)

instance ordFairEither :: (Ord a, Ord b) => Ord (FairEither a b) where
  compare (FairEither t1) (FairEither t2) = t1 `compare` t2

instance showFairEither :: (Show a, Show b) => Show (FairEither a b) where
  show (FairEither t) = "FairEither (" ++ show t ++ ")"

instance enumFairEither :: (Enum a, Enum b) => Enum (FairEither a b) where
  cardinality = fairEitherCardinality f where f (Cardinality sz1) (Cardinality sz2) = Cardinality (sz1 + sz2)

  firstEnum = FairEither firstEnum

  lastEnum = FairEither lastEnum

  succ (FairEither e) = FairEither <$> succ e

  pred (FairEither e) = FairEither <$> pred e

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform 

instance coarbNumber :: CoArbitrary Number where
  coarbitrary = perturbGen  

instance arbPositive :: Arbitrary Positive where
  arbitrary = Positive <$> ((*) maxNumber) <$> uniform

instance coarbPositive :: CoArbitrary Positive where
  coarbitrary (Positive n) = coarbitrary n

instance arbNegative :: Arbitrary Negative where
  arbitrary = Negative <$> ((*) (-maxNumber)) <$> uniform

instance coarbNegative :: CoArbitrary Negative where
  coarbitrary (Negative n) = coarbitrary n

instance arbNonZero :: Arbitrary NonZero where
  arbitrary = do n <- arbitrary
                 b <- arbitrary
                 let sign = if b then 1.0 else -1.0
                 return $ NonZero (n * maxNumber * sign)

instance coarbNonZero :: CoArbitrary NonZero where
  coarbitrary (NonZero n) = coarbitrary n

instance arbSignum :: Arbitrary Signum where
  arbitrary = do b <- arbitrary
                 return $ Signum (if b then 1 else -1)

instance coarbSignum :: CoArbitrary Signum where
  coarbitrary (Signum n) = coarbitrary n

instance arbLastEnum :: (Enum a) => Arbitrary (LastEnum a) where
  arbitrary = LastEnum <$> cardPerturb1 f where
    f (Cardinality sz) = fromJust <<< toEnum <$> chooseInt 0 (sz - 1)

instance coarbLastEnum :: (Enum a) => CoArbitrary (LastEnum a) where
  coarbitrary (LastEnum e) = coarbitrary (fromEnum e)

instance eqLastEnum :: (Eq a) => Eq (LastEnum a) where
  (==) (LastEnum a) (LastEnum b) = a == b

  (/=) a b = not (a == b)

instance ordLastEnum :: (Ord a) => Ord (LastEnum a) where
  compare (LastEnum a) (LastEnum b) = compare a b

instance showLastEnum :: (Show a) => Show (LastEnum a) where
  show (LastEnum a) = "LastEnum " ++ show a

instance enumLastEnum :: (Enum a) => Enum (LastEnum a) where
  cardinality = lastEnumCardinality f where f (Cardinality sz) = Cardinality sz

  firstEnum = LastEnum firstEnum

  lastEnum = LastEnum lastEnum

  pred (LastEnum e) = LastEnum <$> pred e

  succ (LastEnum e) = LastEnum <$> succ e

instance arbFairTuple :: (Enum a, Enum b, Arbitrary b) => Arbitrary (FairTuple a b) where
  arbitrary = do a <- runLastEnum <$> arbitrary
                 b <- arbitrary
                 return <<< FairTuple $ Tuple a b

instance coarbFairTuple :: (Enum a, Enum b, CoArbitrary b) => CoArbitrary (FairTuple a b) where
  coarbitrary (FairTuple (Tuple a b)) = (coarbitrary $ LastEnum a) >>> coarbitrary b

-- | A fair instance for eithers of enums. Ordinarily a compositional arbitrary for
-- | enums would exhibit extreme bias towards leftmost elements.
instance arbFairEither :: (Enum a, Enum b, Arbitrary b) => Arbitrary (FairEither a b) where
  arbitrary = enumFairEitherArb f where
    f (Cardinality sz1) (Cardinality sz2) = 
      do  w <- chooseInt 0 (sz1 + sz2 - 1)
          FairEither <$> if w < sz1 then Left <<< runLastEnum <$> arbitrary else Right <$> arbitrary

instance coarFairEither :: (Enum a, CoArbitrary b) => CoArbitrary (FairEither a b) where
  coarbitrary (FairEither (Left a)) = coarbitrary $ LastEnum a
  coarbitrary (FairEither (Right b)) = coarbitrary b

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n * 2) < 1

instance coarbBoolean :: CoArbitrary Boolean where
  coarbitrary true  = perturbGen 1
  coarbitrary false = perturbGen 2

instance arbChar :: Arbitrary Char where
  arbitrary = charGen

instance coarbChar :: CoArbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbString :: Arbitrary String where
  arbitrary = S.fromCharArray <$> arbitrary

instance coarbString :: CoArbitrary String where
  coarbitrary s = coarbitrary $ (S.charCodeAt 0 <$> S.split "" s)

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ fromJust $ (AlphaNumString <<< S.fromCharArray) <$> sequence (lookup <$> arrNum) where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      lookup x = S.charAt index chars where
        index = Math.min (S.length chars - 1) $ floor (x * (S.length chars))

instance coarbAlphaNumString :: CoArbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1
  coarbitrary (Just a) = coarbitrary a

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f xs) gen

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

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arbitrary
    test (f t)

-- ScopedTypeVariables
cardPerturb1 :: forall f a. (Enum a) => (Cardinality a -> f a) -> f a
cardPerturb1 f = f cardinality

-- ScopedTypeVariables
lastEnumCardinality :: forall a. (Enum a) => (Cardinality a -> Cardinality (LastEnum a)) -> Cardinality (LastEnum a)
lastEnumCardinality f = f cardinality

-- ScopedTypeVariables
fairEitherCardinality :: forall a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> Cardinality (FairEither a b)) -> Cardinality (FairEither a b)
fairEitherCardinality f = f cardinality cardinality

fairTupleCardinality :: forall a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> Cardinality (FairTuple a b)) -> Cardinality (FairTuple a b)
fairTupleCardinality f = f cardinality cardinality

-- ScopedTypeVariables
enumFairEitherArb :: forall a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> Gen (FairEither a b)) -> Gen (FairEither a b)
enumFairEitherArb f = f cardinality cardinality