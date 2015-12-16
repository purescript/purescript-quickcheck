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
Arbitrary ApproxNumber
Coarbitrary ApproxNumber
Eq ApproxNumber
Ord ApproxNumber
Semiring ApproxNumber
ModuloSemiring ApproxNumber
Ring ApproxNumber
DivisionRing ApproxNumber
Num ApproxNumber
```

#### `(=~=)`

``` purescript
(=~=) :: Number -> Number -> Boolean
```

_left-associative / precedence -1_


