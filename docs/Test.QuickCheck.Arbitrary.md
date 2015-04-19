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

#### `Coarbitrary`

``` purescript
class Coarbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```

The `Coarbitrary` class represents types which appear on the left of
an `Arbitrary` function arrow.

To construct an `Arbitrary` instance for the type `a -> b`, we need to
use the input of type `a` to _perturb_ a random generator for `b`. This
is the role of the `coarbitrary` function.

`Coarbitrary` instances can be written using the `perturbGen` function.

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


#### `arbInt`

``` purescript
instance arbInt :: Arbitrary Int
```


#### `coarbInt`

``` purescript
instance coarbInt :: Coarbitrary Int
```


#### `arbString`

``` purescript
instance arbString :: Arbitrary String
```


#### `coarbString`

``` purescript
instance coarbString :: Coarbitrary String
```


#### `arbChar`

``` purescript
instance arbChar :: Arbitrary Char
```


#### `coarbChar`

``` purescript
instance coarbChar :: Coarbitrary Char
```


#### `arbUnit`

``` purescript
instance arbUnit :: Arbitrary Unit
```


#### `coarbUnit`

``` purescript
instance coarbUnit :: Coarbitrary Unit
```


#### `arbOrdering`

``` purescript
instance arbOrdering :: Arbitrary Ordering
```


#### `coarbOrdering`

``` purescript
instance coarbOrdering :: Coarbitrary Ordering
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


#### `arbTuple`

``` purescript
instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
```


#### `coarbTuple`

``` purescript
instance coarbTuple :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b)
```


#### `arbMaybe`

``` purescript
instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)
```


#### `coarbMaybe`

``` purescript
instance coarbMaybe :: (Coarbitrary a) => Coarbitrary (Maybe a)
```


#### `arbEither`

``` purescript
instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
```


#### `coarbEither`

``` purescript
instance coarbEither :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b)
```




