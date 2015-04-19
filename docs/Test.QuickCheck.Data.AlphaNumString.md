# Module Documentation

## Module Test.QuickCheck.Data.AlphaNumString

#### `AlphaNumString`

``` purescript
newtype AlphaNumString
  = AlphaNumString String
```

A newtype for `String` whose `Arbitrary` instance generated random
alphanumeric strings.

#### `arbAlphaNumString`

``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
```


#### `coarbAlphaNumString`

``` purescript
instance coarbAlphaNumString :: Coarbitrary AlphaNumString
```




