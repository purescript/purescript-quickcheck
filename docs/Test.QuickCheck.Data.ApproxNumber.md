# Module Documentation

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
instance coarbitraryApproxNumber :: Coarbitrary ApproxNumber
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




