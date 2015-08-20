## Module Test.QuickCheck.Generic

Generic deriving for `Arbitrary` and `Coarbitrary` instances.

#### `gArbitrary`

``` purescript
gArbitrary :: forall a. (Generic a) => Gen a
```

Generate arbitrary values for any `Generic` data structure.

_Note_: this function _is_ partial, since it does not handle ADTs with zero constructors
such as `Void`.

#### `gCoarbitrary`

``` purescript
gCoarbitrary :: forall a r. (Generic a) => a -> Gen r -> Gen r
```

Perturb a generator using a `Generic` data structure.


