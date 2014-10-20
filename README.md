# Module Documentation

## Module Test.QuickCheck

### Types

    newtype AlphaNumString where
      AlphaNumString :: String -> AlphaNumString

    newtype FairEither a b where
      FairEither :: Either a b -> FairEither a b

    newtype FairTuple a b where
      FairTuple :: Tuple a b -> FairTuple a b

    newtype LastEnum a where
      LastEnum :: a -> LastEnum a

    newtype Negative where
      Negative :: Number -> Negative

    newtype NonZero where
      NonZero :: Number -> NonZero

    newtype Positive where
      Positive :: Number -> Positive

    type QC a = forall eff. Eff (err :: Exception, random :: Random, trace :: Trace | eff) a

    data Result where
      Success :: Result
      Failed :: String -> Result

    newtype Signum where
      Signum :: Number -> Signum


### Type Classes

    class Arbitrary t where
      arbitrary :: Gen t

    class CoArbitrary t where
      coarbitrary :: forall r. t -> Gen r -> Gen r

    class Testable prop where
      test :: prop -> Gen Result


### Type Class Instances

    instance arbAlphaNumString :: Arbitrary AlphaNumString

    instance arbArray :: (Arbitrary a) => Arbitrary [a]

    instance arbBoolean :: Arbitrary Boolean

    instance arbChar :: Arbitrary Char

    instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)

    instance arbFairEither :: (Enum a, Enum b, Arbitrary b) => Arbitrary (FairEither a b)

    instance arbFairTuple :: (Enum a, Enum b, Arbitrary b) => Arbitrary (FairTuple a b)

    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)

    instance arbLastEnum :: (Enum a) => Arbitrary (LastEnum a)

    instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)

    instance arbNegative :: Arbitrary Negative

    instance arbNonZero :: Arbitrary NonZero

    instance arbNumber :: Arbitrary Number

    instance arbPositive :: Arbitrary Positive

    instance arbSignum :: Arbitrary Signum

    instance arbString :: Arbitrary String

    instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)

    instance coarFairEither :: (Enum a, CoArbitrary b) => CoArbitrary (FairEither a b)

    instance coarbAlphaNumString :: CoArbitrary AlphaNumString

    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]

    instance coarbBoolean :: CoArbitrary Boolean

    instance coarbChar :: CoArbitrary Char

    instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)

    instance coarbFairTuple :: (Enum a, Enum b, CoArbitrary b) => CoArbitrary (FairTuple a b)

    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)

    instance coarbLastEnum :: (Enum a) => CoArbitrary (LastEnum a)

    instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)

    instance coarbNegative :: CoArbitrary Negative

    instance coarbNonZero :: CoArbitrary NonZero

    instance coarbNumber :: CoArbitrary Number

    instance coarbPositive :: CoArbitrary Positive

    instance coarbSignum :: CoArbitrary Signum

    instance coarbString :: CoArbitrary String

    instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)

    instance enumFairEither :: (Enum a, Enum b) => Enum (FairEither a b)

    instance enumFairTuple :: (Enum a, Enum b) => Enum (FairTuple a b)

    instance enumLastEnum :: (Enum a) => Enum (LastEnum a)

    instance eqFairEither :: (Eq a, Eq b) => Eq (FairEither a b)

    instance eqFairTuple :: (Eq a, Eq b) => Eq (FairTuple a b)

    instance eqLastEnum :: (Eq a) => Eq (LastEnum a)

    instance eqResult :: Eq Result

    instance monoidFairTuple :: (Monoid a, Monoid b) => Monoid (FairTuple a b)

    instance monoidResult :: Monoid Result

    instance ordFairEither :: (Ord a, Ord b) => Ord (FairEither a b)

    instance ordFairTuple :: (Ord a, Ord b) => Ord (FairTuple a b)

    instance ordLastEnum :: (Ord a) => Ord (LastEnum a)

    instance semigroupFairTuple :: (Semigroup a, Semigroup b) => Semigroup (FairTuple a b)

    instance semigroupResult :: Semigroup Result

    instance showFairEither :: (Show a, Show b) => Show (FairEither a b)

    instance showFairTuple :: (Show a, Show b) => Show (FairTuple a b)

    instance showLastEnum :: (Show a) => Show (LastEnum a)

    instance showResult :: Show Result

    instance testableBoolean :: Testable Boolean

    instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)

    instance testableResult :: Testable Result


### Values

    (<?>) :: Boolean -> String -> Result

    assert :: forall prop. (Testable prop) => prop -> QC Unit

    quickCheck :: forall prop. (Testable prop) => prop -> QC Unit

    quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit

    quickCheckPure :: forall prop. (Testable prop) => Number -> Seed -> prop -> [Result]

    runLastEnum :: forall a. LastEnum a -> a

    smallCheck :: forall prop. (Testable prop) => prop -> QC Unit

    smallCheckPure :: forall prop. (Testable prop) => Number -> prop -> [Result]

    statCheck :: forall prop. (Testable prop) => Number -> prop -> QC Unit

    statCheckPure :: forall prop. (Testable prop) => Seed -> Number -> prop -> Result



