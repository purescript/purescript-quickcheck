## Module Test.QuickCheck.Data.AlphaNumString

#### `AlphaNumString`

``` purescript
newtype AlphaNumString
  = AlphaNumString String
```

A newtype for `String` whose `Arbitrary` instance generated random
alphanumeric strings.

##### Instances
``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
instance coarbAlphaNumString :: Coarbitrary AlphaNumString
```

#### `runAlphaNumString`

``` purescript
runAlphaNumString :: AlphaNumString -> String
```


