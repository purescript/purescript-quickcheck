# Module Documentation

## Module Test.QuickCheck.LCG

### Types

    data Gen a

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

    choose :: Number -> Number -> Gen Number

    chooseInt :: Number -> Number -> Gen Number

    elements :: forall a. a -> [a] -> Gen a

    evalGen :: forall a. Gen a -> GenState -> a

    frequency :: forall a. Tuple Number (Gen a) -> [Tuple Number (Gen a)] -> Gen a

    listOf :: forall a. Gen a -> Gen [a]

    listOf1 :: forall a. Gen a -> Gen (Tuple a [a])

    oneOf :: forall a. Gen a -> [Gen a] -> Gen a

    perturbGen :: forall a. Number -> Gen a -> Gen a

    repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)

    resize :: forall a. Size -> Gen a -> Gen a

    runGen :: forall a. Gen a -> GenState -> GenOut a

    sized :: forall a. (Size -> Gen a) -> Gen a

    stateful :: forall a. (GenState -> Gen a) -> Gen a

    uniform :: Gen Number

    variant :: forall a. LCG -> Gen a -> Gen a

    vectorOf :: forall a. Number -> Gen a -> Gen [a]



