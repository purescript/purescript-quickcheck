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
Arbitrary AlphaNumString
Coarbitrary AlphaNumString
```

#### `runAlphaNumString`

``` purescript
runAlphaNumString :: AlphaNumString -> String
```


