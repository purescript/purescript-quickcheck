## Module Test.QuickCheck.Data.ApproxNumber

#### `ApproxNumber`

``` purescript
newtype ApproxNumber
  = ApproxNumber Number
```

A newtype for `Number` whose `Eq` instance uses an epsilon value to allow
for precision erros when comparing.

##### Instances
``` purescript
instance arbitraryApproxNumber :: Arbitrary ApproxNumber
instance coarbitraryApproxNumber :: Coarbitrary ApproxNumber
instance eqApproxNumber :: Eq ApproxNumber
instance ordApproxNumber :: Ord ApproxNumber
instance semiringApproxNumber :: Semiring ApproxNumber
instance moduloSemiringApproxNumber :: ModuloSemiring ApproxNumber
instance ringApproxNumber :: Ring ApproxNumber
instance divisionRingApproxNumber :: DivisionRing ApproxNumber
instance numApproxNumber :: Num ApproxNumber
```

#### `(=~=)`

``` purescript
(=~=) :: Number -> Number -> Boolean
```


