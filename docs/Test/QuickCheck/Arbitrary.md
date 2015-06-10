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

##### Instances
``` purescript
instance arbBoolean :: Arbitrary Boolean
instance arbNumber :: Arbitrary Number
instance arbInt :: Arbitrary Int
instance arbString :: Arbitrary String
instance arbChar :: Arbitrary Char
instance arbUnit :: Arbitrary Unit
instance arbOrdering :: Arbitrary Ordering
instance arbArray :: (Arbitrary a) => Arbitrary (Array a)
instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b)
instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)
instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
```

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

##### Instances
``` purescript
instance coarbBoolean :: Coarbitrary Boolean
instance coarbNumber :: Coarbitrary Number
instance coarbInt :: Coarbitrary Int
instance coarbString :: Coarbitrary String
instance coarbChar :: Coarbitrary Char
instance coarbUnit :: Coarbitrary Unit
instance coarbOrdering :: Coarbitrary Ordering
instance coarbArray :: (Coarbitrary a) => Coarbitrary (Array a)
instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b)
instance coarbTuple :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b)
instance coarbMaybe :: (Coarbitrary a) => Coarbitrary (Maybe a)
instance coarbEither :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b)
```


