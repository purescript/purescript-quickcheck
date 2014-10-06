# Module Documentation

## Module Test.QuickCheck

### Types

    newtype AlphaNumString where
      AlphaNumString :: String -> AlphaNumString

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

    instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)

    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)

    instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)

    instance arbNegative :: Arbitrary Negative

    instance arbNonZero :: Arbitrary NonZero

    instance arbNumber :: Arbitrary Number

    instance arbPositive :: Arbitrary Positive

    instance arbString :: Arbitrary String

    instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)

    instance coarbAlphaNumString :: CoArbitrary AlphaNumString

    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]

    instance coarbBoolean :: CoArbitrary Boolean

    instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)

    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)

    instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)

    instance coarbNegative :: CoArbitrary Negative

    instance coarbNonZero :: CoArbitrary NonZero

    instance coarbNumber :: CoArbitrary Number

    instance coarbPositive :: CoArbitrary Positive

    instance coarbString :: CoArbitrary String

    instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)

    instance showResult :: Show Result

    instance testableBoolean :: Testable Boolean

    instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)

    instance testableResult :: Testable Result


### Values

    (<?>) :: Boolean -> String -> Result

    quickCheck :: forall prop. (Testable prop) => prop -> QC Unit

    quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit

    quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]

    smallCheck :: forall prop. (Testable prop) => prop -> QC Unit

    smallCheckPure :: forall prop. (Testable prop) => Number -> prop -> [Result]



