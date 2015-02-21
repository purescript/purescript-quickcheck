# Module Documentation

## Module Test.QuickCheck

#### `Arbitrary`

``` purescript
class Arbitrary t where
  arbitrary :: Gen t
```


#### `CoArbitrary`

``` purescript
class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```


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


#### `(<?>)`

``` purescript
(<?>) :: Boolean -> String -> Result
```


#### `arbChar`

``` purescript
instance arbChar :: Arbitrary S.Char
```


#### `coarbChar`

``` purescript
instance coarbChar :: CoArbitrary S.Char
```


#### `arbNumber`

``` purescript
instance arbNumber :: Arbitrary Number
```


#### `coarbNumber`

``` purescript
instance coarbNumber :: CoArbitrary Number
```


#### `arbBoolean`

``` purescript
instance arbBoolean :: Arbitrary Boolean
```


#### `coarbBoolean`

``` purescript
instance coarbBoolean :: CoArbitrary Boolean
```


#### `arbString`

``` purescript
instance arbString :: Arbitrary String
```


#### `coarbString`

``` purescript
instance coarbString :: CoArbitrary String
```


#### `AlphaNumString`

``` purescript
newtype AlphaNumString
  = AlphaNumString String
```


#### `arbAlphaNumString`

``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
```


#### `coarbAlphaNumString`

``` purescript
instance coarbAlphaNumString :: CoArbitrary AlphaNumString
```


#### `arbTuple`

``` purescript
instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
```


#### `coarbTuple`

``` purescript
instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)
```


#### `arbEither`

``` purescript
instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
```


#### `coarbEither`

``` purescript
instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)
```


#### `arbMaybe`

``` purescript
instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)
```


#### `coarbMaybe`

``` purescript
instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)
```


#### `arbFunction`

``` purescript
instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
```


#### `coarbFunction`

``` purescript
instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)
```


#### `arbArray`

``` purescript
instance arbArray :: (Arbitrary a) => Arbitrary [a]
```


#### `coarbArray`

``` purescript
instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]
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


#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]
```


#### `QC`

``` purescript
type QC a = forall eff. Eff (err :: Exception, random :: Random, trace :: Trace | eff) a
```


#### `quickCheck'`

``` purescript
quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
```


#### `quickCheck`

``` purescript
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
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


#### `repeatable`

``` purescript
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
```


#### `stateful`

``` purescript
stateful :: forall a. (GenState -> Gen a) -> Gen a
```


#### `variant`

``` purescript
variant :: forall a. LCG -> Gen a -> Gen a
```


#### `sized`

``` purescript
sized :: forall a. (Size -> Gen a) -> Gen a
```


#### `resize`

``` purescript
resize :: forall a. Size -> Gen a -> Gen a
```


#### `choose`

``` purescript
choose :: Number -> Number -> Gen Number
```


#### `chooseInt`

``` purescript
chooseInt :: Number -> Number -> Gen Number
```


#### `oneOf`

``` purescript
oneOf :: forall a. Gen a -> [Gen a] -> Gen a
```


#### `frequency`

``` purescript
frequency :: forall a. Tuple Number (Gen a) -> [Tuple Number (Gen a)] -> Gen a
```


#### `arrayOf`

``` purescript
arrayOf :: forall a. Gen a -> Gen [a]
```


#### `arrayOf1`

``` purescript
arrayOf1 :: forall a. Gen a -> Gen (Tuple a [a])
```


#### `vectorOf`

``` purescript
vectorOf :: forall a. Number -> Gen a -> Gen [a]
```


#### `elements`

``` purescript
elements :: forall a. a -> [a] -> Gen a
```


#### `runGen`

``` purescript
runGen :: forall a. Gen a -> GenState -> GenOut a
```


#### `evalGen`

``` purescript
evalGen :: forall a. Gen a -> GenState -> a
```


#### `showSample'`

``` purescript
showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (trace :: Trace | r) Unit
```


#### `showSample`

``` purescript
showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit
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