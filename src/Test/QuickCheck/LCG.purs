module Test.QuickCheck.LCG
  ( GenT(..)
  , Gen(..)
  , GenState(..)
  , GenOut(..)
  , Size()
  , Seed()
  , allInArray
  , allInRange
  , arrayOf 
  , arrayOf1 
  , choose 
  , chooseInt 
  , collectAll
  , dropGen 
  , elements 
  , extend
  , foldGen 
  , frequency 
  , infinite
  , oneOf 
  , perturbGen 
  , repeatable 
  , resize 
  , sample 
  , sample' 
  , showSample
  , showSample' 
  , sized 
  , stateful 
  , suchThat
  , suchThatMaybe
  , takeGen 
  , unfoldGen
  , uniform 
  , variant 
  , vectorOf 
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Debug.Trace
import Data.Tuple
import Data.Lazy
import Data.Profunctor
import Data.Monoid
import Data.Monoid.Sum
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Foldable
import Data.Traversable
import qualified Data.Array as A
import Control.Monad.Free
import Control.Monad.Trampoline
import Control.Arrow
import Control.Monad
import Control.Bind
import qualified Math as M
import qualified Data.Machine.Mealy as Mealy

type Size = Number
type Seed  = Number

data GenState = GenState { seed :: Seed, size :: Size }

unGenState :: GenState -> { seed :: Seed, size :: Size }
unGenState (GenState s) = s

stateM :: ({ seed :: Seed, size :: Size } -> { seed :: Seed, size :: Size }) -> GenState -> GenState
stateM f = GenState <<< f <<< unGenState

data GenOut a = GenOut { state :: GenState, value :: a }

unGenOut :: forall a. GenOut a -> { state :: GenState, value :: a }
unGenOut (GenOut v) = v

data GenT f a = GenT (Mealy.MealyT f GenState (GenOut a))

type Gen a = GenT Trampoline a

unGen :: forall f a. GenT f a -> Mealy.MealyT f GenState (GenOut a)
unGen (GenT m) = m

lcgM :: Seed
lcgM = 1103515245 

lcgC :: Seed
lcgC = 12345

lcgN :: Seed
lcgN = 1 `shl` 30

lcgNext :: Seed -> Seed
lcgNext n = (lcgM * n + lcgC) % lcgN

lcgStep :: forall f. (Monad f) => GenT f Seed
lcgStep = GenT $ arr $ \s -> GenOut { state: updateSeedState s, value: (unGenState s).seed } 

uniform :: forall f. (Monad f) => GenT f Seed
uniform = (\n -> n / (1 `shl` 30)) <$> lcgStep

stepGen :: forall f a. (Monad f) => GenState -> GenT f a -> f (Maybe (GenOut (Tuple a (GenT f a))))
stepGen st (GenT m) =   h <$> Mealy.stepMealy st m
                        where h Mealy.Halt        = Nothing
                              h (Mealy.Emit a m)  = Just $ flip Tuple (GenT m) <$> a 

evalGen :: forall f a. (Monad f) => GenT f a -> GenState -> f (Maybe a) 
evalGen g st = h <$> stepGen st g
                where h Nothing                               = Nothing
                      h (Just (GenOut { value = Tuple a _ })) = Just a

pureGen :: forall f a. (Monad f) => (GenState -> GenOut a) -> GenT f a
pureGen f = GenT $ arr f

repeatable' :: forall f a b. (Monad f) => (a -> GenT f b) -> GenT f (a -> f b)
repeatable' f = GenT $ 
  Mealy.pureMealy $ \s -> Mealy.Emit (GenOut { state: s, value: \a -> fromJust <$> evalGen (f a) s }) Mealy.halt

-- | Creates a function generator that will always generate the same output 
-- | for the same input.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = g <$> repeatable' f
               where g f' = \a -> runTrampoline $ f' a

-- | Creates a generator that depends on access to the generator state.
stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a
stateful f = GenT $ do s <- Mealy.take 1 id
                       unGen (f s)

-- | Fixes a generator on a certain variant, given by the specified seed.
variant :: forall f a. (Monad f) => Seed -> GenT f a -> GenT f a
variant n g = GenT $ lmap (stateM (\s -> s { seed = n })) (unGen g)

-- | Creates a generator that depends on the size parameter.
sized :: forall f a. (Monad f) => (Size -> GenT f a) -> GenT f a
sized f = stateful $ \s -> f (unGenState s).size

-- | Resizes the generator so the size parameter passed into the generator 
-- | will be equal to the specified size.
resize :: forall f a. (Monad f) => Size -> GenT f a -> GenT f a
resize sz g = GenT $ lmap (stateM (\s -> s { size = sz })) (unGen g)

-- | Creates a generator that generates real numbers between the specified
-- | inclusive range.
choose :: forall f. (Monad f) => Number -> Number -> GenT f Number
choose a b = (*) (max - min) >>> (+) min <$> uniform 
  where min = M.min a b
        max = M.max a b

-- | Creates a generator that generates integers between the specified 
-- | inclusive range.
chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Number
chooseInt a b = let min = M.ceil  (M.min a b)
                    max = M.floor (M.max a b)
                in  (M.round <<< (+) (min - 0.5) <<< (*) (max - min + 1)) <$> uniform

-- | Creates a generator that chooses another generator from the specified list
-- | at random, and then generates a value with that generator.
oneOf :: forall f a. (Monad f) => GenT f a -> [GenT f a] -> GenT f a
oneOf x xs = do n <- chooseInt 0 (A.length xs)
                if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

-- | Generates elements by the specified frequencies (which will be normalized).
frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> [Tuple Number (GenT f a)] -> GenT f a
frequency x xs = 
  let xxs   = x : xs
      total = runSum $ fold (((Sum <<< fst) <$> xxs) :: [Sum])
      pick n d [] = d
      pick n d ((Tuple k x) : xs) = if n <= k then x else pick (n - k) d xs

  in do n <- chooseInt 1 total
        pick n (snd x) xxs

-- | Creates a generator of elements ranging from 0 to the maximum size.
arrayOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]
arrayOf g = sized $ \n -> 
  do k <- chooseInt 0 n
     vectorOf k g

-- | Creates a generator of elements ranging from 1 to the maximum size.
arrayOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])
arrayOf1 g = sized $ \n ->
  do k  <- chooseInt 0 n
     x  <- g
     xs <- vectorOf (k - 1) g
     return $ Tuple x xs

-- | Creates a generator that generates arrays of some specified size.
vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]
vectorOf n g = unfoldGen f [] (extend n g)
  where f b a = let b' = b <> [a] 
                in  if A.length b' >= n then Tuple [] (Just b') else Tuple b' Nothing

-- | Creates a generator that chooses an element from among a set of elements.
elements :: forall f a. (Monad f) => a -> [a] -> GenT f a
elements x xs = do
  n <- chooseInt 0 (A.length xs)
  pure if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

foreign import float32ToInt32 
  "function float32ToInt32(n) {\
  \  var arr = new ArrayBuffer(4);\
  \  var fv = new Float32Array(arr);\
  \  var iv = new Int32Array(arr);\
  \  fv[0] = n;\
  \  return iv[0];\
  \}" :: Number -> Number

perturbGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
perturbGen n (GenT m) = GenT $ lmap (stateM (\s -> s { seed = perturbNum n s.seed })) m

perturbNum :: Number -> Seed -> Seed
perturbNum n = (+) $ lcgNext (float32ToInt32 n)

updateSeedState :: GenState -> GenState
updateSeedState (GenState s) = GenState { seed: lcgNext s.seed, size: s.size }

updateSeedGen :: forall f a. (Monad f) => GenT f a -> GenT f a
updateSeedGen (GenT m) = GenT $ lmap updateSeedState m

liftMealy :: forall f a. (Monad f) => (Mealy.MealyT f GenState (GenOut a) -> Mealy.MealyT f GenState (GenOut a)) -> (GenT f a -> GenT f a)
liftMealy f = \g -> GenT $ f (unGen g)

-- | Takes the first number of values from the generator. Will turn an infinite
-- | generator into a finite generator.
takeGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
takeGen n = liftMealy $ Mealy.take n

-- | Drops a certain number of values from the generator. May produce
-- | an empty generator if called on a finite generator.
dropGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
dropGen n = liftMealy $ Mealy.drop n

-- | Extends a generator to produce *at least* the specified number of values.
-- | Will not turn a finite generator into an infinite one.
extend :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
extend n (GenT m) = GenT $ loop 0 m
  where m0 = m
        loop i m = Mealy.mealy \st -> 
          let f Mealy.Halt       = if i >= n then pure Mealy.Halt else Mealy.stepMealy st (loop i m0)
              f (Mealy.Emit s m) = pure $ Mealy.Emit s (loop (i + 1) m)

          in  Mealy.stepMealy st m >>= f

-- | Ensures that a given generator can produce an infinite number of values,
-- | assuming it can produce at least one.
infinite :: forall f a. (Monad f) => GenT f a -> GenT f a
infinite = liftMealy $ Mealy.loop

-- | Folds over a generator to produce a value. Either the generator or the 
-- | user-defined function may terminate the fold.
foldGen :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f b
foldGen f b s (GenT m) = loop s m b where
  loop st m b = Mealy.stepMealy st m >>= g
    where g Mealy.Halt                                        = pure b
          g (Mealy.Emit (GenOut { value = a, state = st }) m) = let b' = f b a in maybe (pure b) (loop st m) b'

unfoldGen :: forall f a b c. (Monad f) => (b -> a -> Tuple b (Maybe c)) -> b -> GenT f a -> GenT f c
unfoldGen f b (GenT m) = GenT $ loop m b where
  loop m b = Mealy.mealy $ \st -> Mealy.stepMealy st m >>= g
    where g Mealy.Halt                                        = pure Mealy.Halt
          g (Mealy.Emit (GenOut { value = a, state = st }) m) = case f b a of 
                                                                  Tuple b Nothing  -> Mealy.stepMealy st (loop m b)
                                                                  Tuple b (Just c) -> 
                                                                    let c' = GenOut { value: c, state: st }
                                                                    in  pure $ Mealy.Emit c' (loop m b)

-- FIXME: workaround type inference unification bug
ifThenElse p a b = if p then a else b

-- | Filters a generator to produce only values satisfying the specified 
-- | predicate.
suchThat :: forall f a. (Monad f) => GenT f a -> (a -> Boolean) -> GenT f a
suchThat g p = unfoldGen f unit g where
  f _ a = Tuple unit $ ifThenElse (p a) (Just a) Nothing

-- | Filters a generator to produce only values satisfying the specified 
-- | predicate, but gives up and produces Nothing after the specified number
-- | of attempts.
suchThatMaybe :: forall f a. (Monad f) => Number -> GenT f a -> (a -> Boolean) -> GenT f (Maybe a)
suchThatMaybe n g p = unfoldGen f 0 g where
  f i a = ifThenElse (p a) (Tuple 0 (Just $ Just a)) (ifThenElse (i >= n) (Tuple 0 (Just $ Nothing)) (Tuple (i + 1) Nothing))

-- | A deterministic generator that produces integers from the specified 
-- | inclusive range, in sequence.
allInRange :: forall f a. (Monad f) => Number -> Number -> GenT f Number
allInRange min max = GenT $ go min where 
  go cur = Mealy.pureMealy $ \s -> 
    ifThenElse (cur > max) Mealy.Halt (Mealy.Emit (GenOut { state: s, value: cur }) (go (cur + 1)))

-- | A deterministic generator that produces values from the specified array,
-- | in sequence.
allInArray :: forall f a. (Monad f) => [a] -> GenT f a
allInArray a = GenT $ go 0 where
  go i = Mealy.pureMealy $ \s -> 
    maybe Mealy.Halt (\a -> Mealy.Emit (GenOut { state: s, value: a }) (go (i + 1))) (a A.!! i)

-- | Drains a finite generator of all values. Or blows up if you called it on 
-- | an infinite generator.
collectAll :: forall f a. (Monad f) => GenState -> GenT f a -> f [a]
collectAll = foldGen f []
  where f v a = Just $ v <> [a]

-- | Samples a generator, producing the specified number of values.
sample' :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f [a]
sample' n st g = foldGen f [] st (extend n g)
  where f v a = ifThenElse (A.length v < n) (Just $ v <> [a]) Nothing 

-- | Samples a generator, producing the specified number of values. Uses 
-- | default settings for the initial generator state.
sample :: forall f a. (Monad f) => Number -> GenT f a -> f [a]
sample n = sample' n (GenState { size: 10, seed: 0 })

-- | Shows a sample of values generated from the specified generator.
showSample' :: forall r a. (Show a) => Number -> Gen a -> Eff (trace :: Trace | r) Unit
showSample' n g = print $ runTrampoline $ sample n g

-- | Shows a sample of values generated from the specified generator.
showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit
showSample = showSample' 10

instance semigroupGenState :: Semigroup GenState where
  (<>) (GenState a) (GenState b) = GenState { seed: perturbNum a.seed b.seed, size: b.size }

instance monoidGenState :: Monoid GenState where
  mempty = GenState { seed: 0, size: 10 }

instance semigroupGenOut :: (Semigroup a) => Semigroup (GenOut a) where
  (<>) (GenOut a) (GenOut b) = GenOut { state: a.state <> b.state, value: a.value <> b.value }

instance monoidGenOut :: (Monoid a) => Monoid (GenOut a) where
  mempty = GenOut { state: mempty, value: mempty }

instance functorGenOut :: Functor GenOut where
  (<$>) f (GenOut m) = GenOut { state: m.state, value: f m.value }

instance applyGenOut :: Apply GenOut where
  (<*>) (GenOut f) (GenOut x) = GenOut { state: x.state, value: f.value x.value }

-- GenT instances
instance functorGenT :: (Monad f) => Functor (GenT f) where
  (<$>) f (GenT m) = GenT $ (<$>) f <$> m

instance applyGenT :: (Monad f) => Apply (GenT f) where
  (<*>) f x = GenT $ do f <- unGen f
                        x <- unGen $ updateSeedGen x
                        return $ f <*> x

instance applicativeGenT :: (Monad f) => Applicative (GenT f) where
  pure t = GenT $ arr (\s -> GenOut { state: updateSeedState s, value: t })

instance semigroupGenT :: (Monad f) => Semigroup (GenT f a) where
  (<>) (GenT a) (GenT b) = GenT (a <> b)

instance monoidGenT :: (Monad f) => Monoid (GenT f a) where
  mempty = GenT mempty

instance bindGenT :: (Monad f) => Bind (GenT f) where
  (>>=) (GenT m) f = GenT $ do a <- m
                               unGen $ updateSeedGen (f (unGenOut a).value)

instance monadGenT :: (Monad f) => Monad (GenT f)