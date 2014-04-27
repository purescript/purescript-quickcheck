# Module Documentation

## Module Test.QuickCheck

### Types

    type QC a = forall eff. Eff (err :: Exception Prim.String, random :: Random, trace :: Trace | eff) a

    data Result  where
      Success :: Result 
      Failed :: Prim.String -> Result 


### Type Classes

    class Arbitrary t where
      arbitrary :: Gen t

    class CoArbitrary t where
      coarbitrary :: forall r. t -> Gen r -> Gen r

    class Testable prop where
      test :: prop -> Gen Result


### Type Class Instances

    instance arbArray :: (Arbitrary a) => Arbitrary [a]

    instance arbBoolean :: Arbitrary Prim.Boolean

    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)

    instance arbNumber :: Arbitrary Prim.Number

    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]

    instance coarbBoolean :: CoArbitrary Prim.Boolean

    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)

    instance coarbNumber :: CoArbitrary Prim.Number

    instance showResult :: Show Result

    instance testableBoolean :: Testable Prim.Boolean

    instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)

    instance testableResult :: Testable Result


### Values

    (<?>) :: Prim.Boolean -> Prim.String -> Result

    quickCheck :: forall prop. (Testable prop) => prop -> QC {  }

    quickCheck' :: forall prop. (Testable prop) => Prim.Number -> prop -> QC {  }

    quickCheckPure :: forall prop. (Testable prop) => Prim.Number -> Prim.Number -> prop -> [Result]

    repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)


## Module Test.QuickCheck.LCG

### Types

    data Gen a where
      Gen :: LCG -> { newSeed :: LCG, value :: a } -> Gen a

    type LCG  = Prim.Number


### Type Class Instances

    instance applicativeGen :: Applicative Gen

    instance applyGen :: Apply Gen

    instance bindGen :: Bind Gen

    instance functorGen :: Functor Gen

    instance monadGen :: Monad Gen


### Values

    evalGen :: forall a. Gen a -> LCG -> a

    float32ToInt32 :: Prim.Number -> Prim.Number

    lcgC :: Prim.Number

    lcgM :: Prim.Number

    lcgN :: Prim.Number

    lcgNext :: Prim.Number -> Prim.Number

    lcgStep :: Gen Prim.Number

    perturbGen :: forall a. Prim.Number -> Gen a -> Gen a

    randomSeed :: forall eff. Eff (random :: Random | eff) Prim.Number

    runGen :: forall a. Gen a -> LCG -> { newSeed :: LCG, value :: a }

    uniform :: Gen Prim.Number