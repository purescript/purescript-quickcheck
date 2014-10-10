# Module Documentation

## Module Test.QuickCheck.LCG

### Types

    type Gen a = GenT Trampoline a

    data GenOut a where
      GenOut :: { value :: a, state :: GenState } -> GenOut a

    data GenState where
      GenState :: { size :: Size, seed :: Seed } -> GenState

    data GenT f a where
      GenT :: Mealy.MealyT f GenState (GenOut a) -> GenT f a

    type Seed  = Number

    type Size  = Number


### Type Class Instances

    instance altGenT :: (Monad f) => Alt (GenT f)

    instance alternativeGenT :: (Monad f) => Alternative (GenT f)

    instance applicativeGenT :: (Monad f) => Applicative (GenT f)

    instance applyGenOut :: Apply GenOut

    instance applyGenT :: (Monad f) => Apply (GenT f)

    instance bindGenT :: (Monad f) => Bind (GenT f)

    instance functorGenOut :: Functor GenOut

    instance functorGenT :: (Monad f) => Functor (GenT f)

    instance monadGenT :: (Monad f) => Monad (GenT f)

    instance monadPlusGenT :: (Monad f) => MonadPlus (GenT f)

    instance monoidGenOut :: (Monoid a) => Monoid (GenOut a)

    instance monoidGenState :: Monoid GenState

    instance monoidGenT :: (Monad f) => Monoid (GenT f a)

    instance plusGenT :: (Monad f) => Plus (GenT f)

    instance semigroupGenOut :: (Semigroup a) => Semigroup (GenOut a)

    instance semigroupGenState :: Semigroup GenState

    instance semigroupGenT :: (Monad f) => Semigroup (GenT f a)


### Values

    allInArray :: forall f a. (Monad f) => [a] -> GenT f a

    allInRange :: forall f a. (Monad f) => Number -> Number -> GenT f Number

    arrayOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]

    arrayOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])

    choose :: forall f. (Monad f) => Number -> Number -> GenT f Number

    chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Number

    collectAll :: forall f a. (Monad f) => GenState -> GenT f a -> f [a]

    dropGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    elements :: forall f a. (Monad f) => a -> [a] -> GenT f a

    extend :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    foldGen :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f b

    foldGen' :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f (Tuple b (GenT f a))

    frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> [Tuple Number (GenT f a)] -> GenT f a

    infinite :: forall f a. (Monad f) => GenT f a -> GenT f a

    oneOf :: forall f a. (Monad f) => GenT f a -> [GenT f a] -> GenT f a

    perms :: forall f a. (Monad f) => [a] -> GenT f [a]

    perturbGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)

    resize :: forall f a. (Monad f) => Size -> GenT f a -> GenT f a

    runGen :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f (Tuple [a] (GenT f a))

    sample :: forall f a. (Monad f) => Number -> GenT f a -> f [a]

    sample' :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f [a]

    showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit

    showSample' :: forall r a. (Show a) => Number -> Gen a -> Eff (trace :: Trace | r) Unit

    sized :: forall f a. (Monad f) => (Size -> GenT f a) -> GenT f a

    stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a

    suchThat :: forall f a. (Monad f) => GenT f a -> (a -> Boolean) -> GenT f a

    suchThatMaybe :: forall f a. (Monad f) => Number -> GenT f a -> (a -> Boolean) -> GenT f (Maybe a)

    takeGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    transGen :: forall f a b c. (Monad f) => (b -> a -> Tuple b (Maybe c)) -> b -> GenT f a -> GenT f c

    uniform :: forall f. (Monad f) => GenT f Seed

    variant :: forall f a. (Monad f) => Seed -> GenT f a -> GenT f a

    vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]



