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
Arbitrary Boolean
Arbitrary Number
Arbitrary Int
Arbitrary String
Arbitrary Char
Arbitrary Unit
Arbitrary Ordering
(Arbitrary a) => Arbitrary (Array a)
(Coarbitrary a, Arbitrary b) => Arbitrary (a -> b)
(Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
(Arbitrary a) => Arbitrary (Maybe a)
(Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
(Arbitrary a) => Arbitrary (List a)
(Arbitrary a) => Arbitrary (Identity a)
(Arbitrary a) => Arbitrary (Lazy a)
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
Coarbitrary Boolean
Coarbitrary Number
Coarbitrary Int
Coarbitrary String
Coarbitrary Char
Coarbitrary Unit
Coarbitrary Ordering
(Coarbitrary a) => Coarbitrary (Array a)
(Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b)
(Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b)
(Coarbitrary a) => Coarbitrary (Maybe a)
(Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b)
(Coarbitrary a) => Coarbitrary (List a)
(Coarbitrary a) => Coarbitrary (Identity a)
(Coarbitrary a) => Coarbitrary (Lazy a)
```


