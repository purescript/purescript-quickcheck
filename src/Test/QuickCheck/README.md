# Module Documentation

## Module Test.QuickCheck.LCG

### Types

    data Gen a where
      Gen :: GenState -> GenOut a -> Gen a

    type GenOut a = { value :: a, state :: GenState }

    type GenState  = { size :: Size, newSeed :: LCG }

    type LCG  = Number

    type Size  = Number


### Type Class Instances

    instance applicativeGen :: Applicative Gen

    instance applyGen :: Apply Gen

    instance bindGen :: Bind Gen

    instance functorGen :: Functor Gen

    instance monadGen :: Monad Gen


### Values

    evalGen :: forall a. Gen a -> GenState -> a

    float32ToInt32 :: Number -> Number

    lcgC :: Number

    lcgM :: Number

    lcgN :: Number

    lcgNext :: Number -> Number

    lcgStep :: Gen Number

    perturbGen :: forall a. Number -> Gen a -> Gen a

    randomSeed :: forall eff. Eff (random :: Random | eff) Number

    resize :: forall a. Number -> Gen a -> Gen a

    runGen :: forall a. Gen a -> GenState -> GenOut a

    sized :: forall a. (Number -> Gen a) -> Gen a

    uniform :: Gen Number



