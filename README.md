# Module Documentation

## Module Test.QuickCheck

#### `QC`

``` purescript
type QC a = forall eff. Eff (err :: Exception, random :: Random, trace :: Trace | eff) a
```


#### `quickCheck`

``` purescript
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
```


#### `quickCheck'`

``` purescript
quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
```


#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]
```


#### `(<?>)`

``` purescript
(<?>) :: Boolean -> String -> Result
```

Creates a `Result` based on a boolean with a potential failure message for
when the boolean is `false`.

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


#### `Coarbitrary`

``` purescript
class Coarbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```


#### `arbBoolean`

``` purescript
instance arbBoolean :: Arbitrary Boolean
```


#### `coarbBoolean`

``` purescript
instance coarbBoolean :: Coarbitrary Boolean
```


#### `arbNumber`

``` purescript
instance arbNumber :: Arbitrary Number
```


#### `coarbNumber`

``` purescript
instance coarbNumber :: Coarbitrary Number
```


#### `arbString`

``` purescript
instance arbString :: Arbitrary String
```


#### `coarbString`

``` purescript
instance coarbString :: Coarbitrary String
```


#### `arbArray`

``` purescript
instance arbArray :: (Arbitrary a) => Arbitrary [a]
```


#### `coarbArray`

``` purescript
instance coarbArray :: (Coarbitrary a) => Coarbitrary [a]
```


#### `arbFunction`

``` purescript
instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b)
```


#### `coarbFunction`

``` purescript
instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b)
```


#### `arbAlphaNumString`

``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
```


#### `coarbAlphaNumString`

``` purescript
instance coarbAlphaNumString :: Coarbitrary AlphaNumString
```



## Module Test.QuickCheck.Gen

#### `LCG`

``` purescript
type LCG = Number
```


#### `Size`

``` purescript
type Size = Number
```


#### `GenState`

``` purescript
type GenState = { size :: Size, newSeed :: LCG }
```


#### `GenOut`

``` purescript
type GenOut a = { value :: a, state :: GenState }
```


#### `Gen`

``` purescript
data Gen a
```


#### `runGen`

``` purescript
runGen :: forall a. Gen a -> GenState -> GenOut a
```


#### `evalGen`

``` purescript
evalGen :: forall a. Gen a -> GenState -> a
```


#### `repeatable`

``` purescript
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
```


#### `stateful`

``` purescript
stateful :: forall a. (GenState -> Gen a) -> Gen a
```


#### `uniform`

``` purescript
uniform :: Gen Number
```


#### `perturbGen`

``` purescript
perturbGen :: forall a. Number -> Gen a -> Gen a
```


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



## Module Test.QuickCheck.Testable

#### `Result`

``` purescript
data Result
  = Success 
  | Failed String
```


#### `showResult`

``` purescript
instance showResult :: Show Result
```


#### `Testable`

``` purescript
class Testable prop where
  test :: prop -> Gen Result
```


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