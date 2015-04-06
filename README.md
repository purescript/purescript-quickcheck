# Module Documentation

## Module Test.QuickCheck


This module is a partial port of the Haskell QuickCheck library.

QuickCheck provides a way to write _property-based_ tests.

The `Arbitrary` and `CoArbitrary` type classes allow us to create
random data with which we can run our tests. This module provides
instances of both classes for PureScript's core data structures,
as well as functions for writing new instances.

Test suites can use the `quickCheck` and `quickCheckPure` functions
to test properties.

For example:

```purescript
main = quickCheck \n -> n + 1 > n
```

#### `QC`

``` purescript
type QC a = forall eff. Eff (err :: Exception, random :: Random, trace :: Trace | eff) a
```

A type synonym which represents the effects used by the `quickCheck` function.

#### `quickCheck`

``` purescript
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
```

Test a property.

This function generates a new random seed, runs 100 tests and
prints the test results to the console.

#### `quickCheck'`

``` purescript
quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
```

A variant of the `quickCheck` function which accepts an extra parameter
representing the number of tests which should be run.

#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]
```

Test a property, returning all test results as an array.

The first argument is the _random seed_ to be passed to the random generator.
The second argument is the number of tests to run.

#### `Result`

``` purescript
data Result
  = Success 
  | Failed String
```

The result of a test: success or failure (with an error message).

#### `showResult`

``` purescript
instance showResult :: Show Result
```


#### `Testable`

``` purescript
class Testable prop where
  test :: prop -> Gen Result
```

The `Testable` class represents _testable properties_.

A testable property is a function of zero or more `Arbitrary` arguments,
returning a `Boolean` or `Result`.

Testable properties can be passed to the `quickCheck` function.

#### `testableResult`

``` purescript
instance testableResult :: Testable Result
```


#### `testableBoolean`

``` purescript
instance testableBoolean :: Testable Boolean
```


#### `testableFunction`

``` purescript
instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)
```


#### `(<?>)`

``` purescript
(<?>) :: Boolean -> String -> Result
```

This operator attaches an error message to a failed test.

For example:

```purescript
test x = myProperty x <?> ("myProperty did not hold for " <> show x)
```

#### `(===)`

``` purescript
(===) :: forall a b. (Eq a, Show a) => a -> a -> Result
```

Self-documenting equality assertion

#### `(/==)`

``` purescript
(/==) :: forall a b. (Eq a, Show a) => a -> a -> Result
```

Self-documenting inequality assertion


## Module Test.QuickCheck.Arbitrary

#### `Arbitrary`

``` purescript
class Arbitrary t where
  arbitrary :: Gen t
```

The `Arbitrary` class represents those types whose values can be
_randomly-generated_.

`arbitrary` uses the `Gen` monad to express a random generator for
the type `t`. Combinators in the `Test.QuickCheck.Gen`
module can be used to construct random generators.

#### `CoArbitrary`

``` purescript
class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```

The `CoArbitrary` class represents types which appear on the left of
an `Arbitrary` function arrow.

To construct an `Arbitrary` instance for the type `a -> b`, we need to
use the input of type `a` to _perturb_ a random generator for `b`. This
is the role of the `coarbitrary` function.

`CoArbitrary` instances can be written using the `perturbGen` function.

#### `arbBoolean`

``` purescript
instance arbBoolean :: Arbitrary Boolean
```


#### `coarbBoolean`

``` purescript
instance coarbBoolean :: CoArbitrary Boolean
```


#### `arbNumber`

``` purescript
instance arbNumber :: Arbitrary Number
```


#### `coarbNumber`

``` purescript
instance coarbNumber :: CoArbitrary Number
```


#### `arbString`

``` purescript
instance arbString :: Arbitrary String
```


#### `coarbString`

``` purescript
instance coarbString :: CoArbitrary String
```


#### `arbChar`

``` purescript
instance arbChar :: Arbitrary Char
```


#### `coarbChar`

``` purescript
instance coarbChar :: CoArbitrary Char
```


#### `arbUnit`

``` purescript
instance arbUnit :: Arbitrary Unit
```


#### `coarbUnit`

``` purescript
instance coarbUnit :: CoArbitrary Unit
```


#### `arbOrdering`

``` purescript
instance arbOrdering :: Arbitrary Ordering
```


#### `coarbOrdering`

``` purescript
instance coarbOrdering :: CoArbitrary Ordering
```


#### `arbArray`

``` purescript
instance arbArray :: (Arbitrary a) => Arbitrary [a]
```


#### `coarbArray`

``` purescript
instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]
```


#### `arbFunction`

``` purescript
instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
```


#### `coarbFunction`

``` purescript
instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)
```


#### `arbTuple`

``` purescript
instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
```


#### `coarbTuple`

``` purescript
instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)
```


#### `arbMaybe`

``` purescript
instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)
```


#### `coarbMaybe`

``` purescript
instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)
```


#### `arbEither`

``` purescript
instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
```


#### `coarbEither`

``` purescript
instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)
```



## Module Test.QuickCheck.Data.AlphaNumString

#### `AlphaNumString`

``` purescript
newtype AlphaNumString
  = AlphaNumString String
```

A newtype for `String` whose `Arbitrary` instance generated random
alphanumeric strings.

#### `arbAlphaNumString`

``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
```


#### `coarbAlphaNumString`

``` purescript
instance coarbAlphaNumString :: CoArbitrary AlphaNumString
```



## Module Test.QuickCheck.Data.ApproxNumber

#### `ApproxNumber`

``` purescript
newtype ApproxNumber
  = ApproxNumber Number
```

A newtype for `Number` whose `Eq` instance uses an epsilon value to allow
for precision erros when comparing.

#### `(=~=)`

``` purescript
(=~=) :: Number -> Number -> Boolean
```

#### `arbitraryApproxNumber`

``` purescript
instance arbitraryApproxNumber :: Arbitrary ApproxNumber
```


#### `coarbitraryApproxNumber`

``` purescript
instance coarbitraryApproxNumber :: CoArbitrary ApproxNumber
```


#### `eqApproxNumber`

``` purescript
instance eqApproxNumber :: Eq ApproxNumber
```


#### `ordApproxNumber`

``` purescript
instance ordApproxNumber :: Ord ApproxNumber
```


#### `semiringApproxNumber`

``` purescript
instance semiringApproxNumber :: Semiring ApproxNumber
```


#### `moduloSemiringApproxNumber`

``` purescript
instance moduloSemiringApproxNumber :: ModuloSemiring ApproxNumber
```


#### `ringApproxNumber`

``` purescript
instance ringApproxNumber :: Ring ApproxNumber
```


#### `divisionRingApproxNumber`

``` purescript
instance divisionRingApproxNumber :: DivisionRing ApproxNumber
```


#### `numApproxNumber`

``` purescript
instance numApproxNumber :: Num ApproxNumber
```



## Module Test.QuickCheck.Gen


This module defines the random generator monad used by the `Test.QuickCheck`
module, as well as helper functions for constructing random generators.

#### `LCG`

``` purescript
type LCG = Number
```

A seed for the random number generator

#### `Size`

``` purescript
type Size = Number
```

Tests are parameterized by the `Size` of the randomly-generated data,
the meaning of which depends on the particular generator used.

#### `GenState`

``` purescript
type GenState = { size :: Size, newSeed :: LCG }
```

The state of the random generator monad

#### `GenOut`

``` purescript
type GenOut a = { value :: a, state :: GenState }
```

The output of the random generator monad

#### `Gen`

``` purescript
data Gen a
```

The random generator monad

`Gen` is a state monad which encodes a linear congruential generator.

#### `repeatable`

``` purescript
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
```

Create a random generator for a function type.

#### `stateful`

``` purescript
stateful :: forall a. (GenState -> Gen a) -> Gen a
```

Create a random generator which uses the generator state explicitly.

#### `variant`

``` purescript
variant :: forall a. LCG -> Gen a -> Gen a
```

Modify a random generator by setting a new random seed.

#### `sized`

``` purescript
sized :: forall a. (Size -> Gen a) -> Gen a
```

Create a random generator which depends on the size parameter.

#### `resize`

``` purescript
resize :: forall a. Size -> Gen a -> Gen a
```

Modify a random generator by setting a new size parameter.

#### `choose`

``` purescript
choose :: Number -> Number -> Gen Number
```

Create a random generator which samples a range of `Number`s i
with uniform probability.

#### `chooseInt`

``` purescript
chooseInt :: Number -> Number -> Gen Number
```

Create a random generator which chooses an integer from a range.

#### `oneOf`

``` purescript
oneOf :: forall a. Gen a -> [Gen a] -> Gen a
```

Create a random generator which selects and executes a random generator from
a non-empty collection of random generators with uniform probability.

#### `frequency`

``` purescript
frequency :: forall a. Tuple Number (Gen a) -> [Tuple Number (Gen a)] -> Gen a
```

Create a random generator which selects and executes a random generator from
a non-empty, weighted collection of random generators.

#### `arrayOf`

``` purescript
arrayOf :: forall a. Gen a -> Gen [a]
```

Create a random generator which generates an array of random values.

#### `arrayOf1`

``` purescript
arrayOf1 :: forall a. Gen a -> Gen (Tuple a [a])
```

Create a random generator which generates a non-empty array of random values.

#### `vectorOf`

``` purescript
vectorOf :: forall a. Number -> Gen a -> Gen [a]
```

Create a random generator which generates a vector of random values of a specified size.

#### `elements`

``` purescript
elements :: forall a. a -> [a] -> Gen a
```

Create a random generator which selects a value from a non-empty collection with
uniform probability.

#### `runGen`

``` purescript
runGen :: forall a. Gen a -> GenState -> GenOut a
```

Run a random generator

#### `evalGen`

``` purescript
evalGen :: forall a. Gen a -> GenState -> a
```

Run a random generator, keeping only the randomly-generated result

#### `showSample'`

``` purescript
showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (trace :: Trace | r) Unit
```

Print a random sample to the console

#### `showSample`

``` purescript
showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit
```

Print a random sample of 10 values to the console

#### `uniform`

``` purescript
uniform :: Gen Number
```

A random generator which approximates a uniform random variable on `[0, 1]`

#### `perturbGen`

``` purescript
perturbGen :: forall a. Number -> Gen a -> Gen a
```

Perturb a random generator by modifying the current seed

#### `functorGen`

``` purescript
instance functorGen :: Functor Gen
```


#### `applyGen`

``` purescript
instance applyGen :: Apply Gen
```


#### `applicativeGen`

``` purescript
instance applicativeGen :: Applicative Gen
```


#### `bindGen`

``` purescript
instance bindGen :: Bind Gen
```


#### `monadGen`

``` purescript
instance monadGen :: Monad Gen
```