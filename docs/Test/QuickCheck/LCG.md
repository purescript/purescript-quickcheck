## Module Test.QuickCheck.LCG

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

The *modulus*: a magic constant for the linear congruential generator.
It is equal to 2^31 - 1, a Mersenne prime. It is useful for this value to
be prime, because then the requirement of the initial seed being coprime
to the modulus is satisfied when the seed is between 1 and lcgN - 1.

#### `lcgPerturb`

``` purescript
lcgPerturb :: Number -> Seed -> Seed
```

Perturb a seed value

#### `lcgNext`

``` purescript
lcgNext :: Seed -> Seed
```

Step the linear congruential generator

#### `randomSeed`

``` purescript
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
```

Create a random seed

#### `Seed`

``` purescript
newtype Seed
```

A seed for the linear congruential generator. We omit a `Semiring`
instance because there is no `zero` value, as 0 is not an acceptable
seed for the generator.

##### Instances
``` purescript
Show Seed
Eq Seed
```

#### `mkSeed`

``` purescript
mkSeed :: Int -> Seed
```

#### `runSeed`

``` purescript
runSeed :: Seed -> Int
```


