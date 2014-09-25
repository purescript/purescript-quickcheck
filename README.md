# Module Documentation

## Module Test.QuickCheck

### Types

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

    instance arbArray :: (Arbitrary a) => Arbitrary [a]

    instance arbBoolean :: Arbitrary Boolean

    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)

    instance arbNumber :: Arbitrary Number

    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]

    instance coarbBoolean :: CoArbitrary Boolean

    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)

    instance coarbNumber :: CoArbitrary Number

    instance showResult :: Show Result

    instance testableBoolean :: Testable Boolean

    instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)

    instance testableResult :: Testable Result


### Values

    (<?>) :: Boolean -> String -> Result

    quickCheck :: forall prop. (Testable prop) => prop -> QC Unit

    quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit

    quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]

    repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)


## Module Test.QuickCheck.LCG

### Types

    data Gen a where
      Gen :: LCG -> { newSeed :: LCG, value :: a } -> Gen a

    type LCG  = Number


### Type Class Instances

    instance applicativeGen :: Applicative Gen

    instance applyGen :: Apply Gen

    instance bindGen :: Bind Gen

    instance functorGen :: Functor Gen

    instance monadGen :: Monad Gen


### Values

    evalGen :: forall a. Gen a -> LCG -> a

    float32ToInt32 :: Number -> Number

    lcgC :: Number

    lcgM :: Number

    lcgN :: Number

    lcgNext :: Number -> Number

    lcgStep :: Gen Number

    perturbGen :: forall a. Number -> Gen a -> Gen a

    randomSeed :: forall eff. Eff (random :: Random | eff) Number

    runGen :: forall a. Gen a -> LCG -> { newSeed :: LCG, value :: a }

    uniform :: Gen Number