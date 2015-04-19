# Module Documentation

## Module Test.QuickCheck.LCG

#### `Seed`

``` purescript
type Seed = Int
```


#### `lcgM`

``` purescript
lcgM :: Int
```

A magic constant for the linear congruential generator

#### `lcgC`

``` purescript
lcgC :: Int
```

A magic constant for the linear congruential generator

#### `lcgN`

``` purescript
lcgN :: Int
```

A magic constant for the linear congruential generator

#### `lcgNext`

``` purescript
lcgNext :: Int -> Int
```

Step the linear congruential generator

#### `randomSeed`

``` purescript
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
```




