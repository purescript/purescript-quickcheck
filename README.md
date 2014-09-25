# Module Documentation

## Module Test.QuickCheck

### Types

    newtype AlphaNumString where
      AlphaNumString :: String -> AlphaNumString

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

    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)

    instance arbNumber :: Arbitrary Number

    instance arbString :: Arbitrary String

    instance coarbAlphaNumString :: CoArbitrary AlphaNumString

    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]

    instance coarbBoolean :: CoArbitrary Boolean

    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)

    instance coarbNumber :: CoArbitrary Number

    instance coarbString :: CoArbitrary String

    instance showResult :: Show Result

    instance testableBoolean :: Testable Boolean

    instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)

    instance testableResult :: Testable Result


### Values

    (<?>) :: Boolean -> String -> Result

    quickCheck :: forall prop. (Testable prop) => prop -> QC Unit

    quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit

    quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]

    quickCheckPure' :: forall prop. (Testable prop) => GenState -> Number -> prop -> [Result]



