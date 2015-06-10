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
type QC a = forall eff. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) a
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
quickCheck' :: forall prop. (Testable prop) => Int -> prop -> QC Unit
```

A variant of the `quickCheck` function which accepts an extra parameter
representing the number of tests which should be run.

#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. (Testable prop) => Int -> Int -> prop -> List Result
```

Test a property, returning all test results as an array.

The first argument is the _random seed_ to be passed to the random generator.
The second argument is the number of tests to run.

#### `Testable`

``` purescript
class Testable prop where
  test :: prop -> Gen Result
```

The `Testable` class represents _testable properties_.

A testable property is a function of zero or more `Arbitrary` arguments,
returning a `Boolean` or `Result`.

Testable properties can be passed to the `quickCheck` function.

##### Instances
``` purescript
instance testableResult :: Testable Result
instance testableBoolean :: Testable Boolean
instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)
```

#### `Result`

``` purescript
data Result
  = Success
  | Failed String
```

The result of a test: success or failure (with an error message).

##### Instances
``` purescript
instance testableResult :: Testable Result
instance showResult :: Show Result
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


