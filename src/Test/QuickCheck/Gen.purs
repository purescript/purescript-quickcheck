module Test.QuickCheck.Gen
  ( Gen()
  , GenState()
  , GenOut()
  , Size()
  , LCG()
  , runGen
  , evalGen
  , perturbGen
  , repeatable
  , stateful
  , uniform
  ) where

type LCG = Number
type Size = Number

type GenState = { newSeed :: LCG, size :: Size }

type GenOut a = { state :: GenState, value :: a }

data Gen a = Gen (GenState -> GenOut a)

runGen :: forall a. Gen a -> GenState -> GenOut a
runGen (Gen f) = f

evalGen :: forall a. Gen a -> GenState -> a
evalGen gen st = (runGen gen st).value

repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ \s -> { value: \a -> (runGen (f a) s).value, state: s }

stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = Gen (\s -> runGen (f s) s)

--
-- Magic Numbers
--

lcgM :: Number
lcgM = 1103515245

lcgC :: Number
lcgC = 12345

lcgN :: Number
lcgN = 1 `shl` 30

lcgNext :: Number -> Number
lcgNext n = (lcgM * n + lcgC) % lcgN

lcgStep :: Gen Number
lcgStep = Gen f where
  f s = { value: s.newSeed, state: s { newSeed = lcgNext s.newSeed } }

uniform :: Gen Number
uniform = (\n -> n / (1 `shl` 30)) <$> lcgStep

foreign import float32ToInt32
  """
  function float32ToInt32(n) {
    var arr = new ArrayBuffer(4);
    var fv = new Float32Array(arr);
    var iv = new Int32Array(arr);
    fv[0] = n;
    return iv[0];
  }
  """ :: Number -> Number

perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n (Gen f) = Gen $ \s -> f (s { newSeed = lcgNext (float32ToInt32 n) + s.newSeed })

instance functorGen :: Functor Gen where
  (<$>) f (Gen g) = Gen $ \s -> case g s of
    { value = value, state = state } -> { value: f value, state: state }

instance applyGen :: Apply Gen where
  (<*>) (Gen f) (Gen x) = Gen $ \s ->
    case f s of
      { value = f', state = s' } -> case x s' of
        { value = x', state = s'' } -> { value: f' x', state: s'' }

instance applicativeGen :: Applicative Gen where
  pure a = Gen (\s -> { value: a, state: s })

instance bindGen :: Bind Gen where
  (>>=) (Gen f) g = Gen $ \s -> case f s of
    { value = value, state = state } -> runGen (g value) state

instance monadGen :: Monad Gen
