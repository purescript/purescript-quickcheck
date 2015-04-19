# Module Documentation

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


#### `arbInt`

``` purescript
instance arbInt :: Arbitrary Int
```


#### `coarbInt`

``` purescript
instance coarbInt :: CoArbitrary Int
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




