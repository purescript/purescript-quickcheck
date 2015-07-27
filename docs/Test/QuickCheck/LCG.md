## Module Test.QuickCheck.LCG

#### `Seed`

``` purescript
type Seed = Int
```

#### `lcgM`

``` purescript
lcgM :: Int
```

The *multiplier*: a magic constant for the linear congruential generator

#### `lcgC`

``` purescript
lcgC :: Int
```

The *increment*: a magic constant for the linear congruential generator

#### `lcgN`

``` purescript
lcgN :: Int
```

The *modulus*: a magic constant for the linear congruential generator

#### `lcgNext`

``` purescript
lcgNext :: Int -> Int
```

Step the linear congruential generator

#### `randomSeed`

``` purescript
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
```

Create a random seed


