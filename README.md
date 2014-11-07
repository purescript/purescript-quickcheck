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

    instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)

    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)

    instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)

    instance arbNumber :: Arbitrary Number

    instance arbString :: Arbitrary String

    instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)

    instance coarbAlphaNumString :: CoArbitrary AlphaNumString

    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]

    instance coarbBoolean :: CoArbitrary Boolean

    instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)

    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)

    instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)

    instance coarbNumber :: CoArbitrary Number

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


## Module Test.QuickCheck.Gen

### Types

    data Gen a

    type GenOut a = { value :: a, state :: GenState }

    type GenState = { size :: Size, newSeed :: LCG }

    type LCG = Number

    type Size = Number


### Type Class Instances

    instance applicativeGen :: Applicative Gen

    instance applyGen :: Apply Gen

    instance bindGen :: Bind Gen

    instance functorGen :: Functor Gen

    instance monadGen :: Monad Gen


### Values

    arrayOf :: forall a. Gen a -> Gen [a]

    arrayOf1 :: forall a. Gen a -> Gen (Tuple a [a])

    choose :: Number -> Number -> Gen Number

    chooseInt :: Number -> Number -> Gen Number

    elements :: forall a. a -> [a] -> Gen a

    evalGen :: forall a. Gen a -> GenState -> a

    frequency :: forall a. Tuple Number (Gen a) -> [Tuple Number (Gen a)] -> Gen a

    oneOf :: forall a. Gen a -> [Gen a] -> Gen a

    perturbGen :: forall a. Number -> Gen a -> Gen a

    repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)

    resize :: forall a. Size -> Gen a -> Gen a

    runGen :: forall a. Gen a -> GenState -> GenOut a

    showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit

    showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (trace :: Trace | r) Unit

    sized :: forall a. (Size -> Gen a) -> Gen a

    stateful :: forall a. (GenState -> Gen a) -> Gen a

    uniform :: Gen Number

    variant :: forall a. LCG -> Gen a -> Gen a

    vectorOf :: forall a. Number -> Gen a -> Gen [a]