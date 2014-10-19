module Test.QuickCheck.Perturb 
  ( Attempts(..)
  , Perturb
  , perturb
  , dist
  , dims
  , searchIn'
  , searchIn
  ) where

  import Test.QuickCheck.LCG
  import Test.QuickCheck
  
  import Data.Traversable
  import Data.Foldable
  import Data.Tuple
  import Data.Monoid
  import Data.Either
  import Data.Maybe
  import Data.Maybe.Unsafe
  import Data.Monoid.Sum
  import Data.Enum
  import qualified Data.String as S
  import qualified Data.Array as A

  import Math

  import Data.Function

  newtype Attempts = Attempts Number

  -- | The class for things which can be perturbed.
  -- |
  -- | Laws:  
  -- |   forall a, 0 >= n <= 1:  
  -- |   ((>=) n) <<< dist a <$> (perturb n a) must be an infinite generator of `true` values.
  class Perturb a where
    perturb :: Number -> a -> Gen a

    dist :: a -> a -> Number

    dims :: a -> Number

  -- | Given one example, searches for other examples that satisfy a provided
  -- | boolean predicate.
  -- | 
  -- | The search operates out-to-in, in an attempt to find examples that are 
  -- | as far removed from the provided example as possible. The sampling size
  -- | parameter determines how many samples to take at every level of 
  -- | searching, while the attempts parameter determines how many levels.
  searchIn' :: forall a. (Perturb a) => Attempts -> Number -> (a -> Boolean) -> a -> Gen a
  searchIn' (Attempts k) n f a = search0 k 1 
    where search0 k d = ifThenElse (k <= 0) mempty
                        (do a' <- find f <$> (takeGen 1 $ chunked n (perturb d a))
                            fromMaybe mempty (pure <$> a') <> search0 (k - 1) (d / 2))
  

  -- | The same as search', but uses defaults for attempt count and sample size.
  -- | Will search a total of 10,000 examples before giving up.
  searchIn :: forall a. (Perturb a) => (a -> Boolean) -> a -> Gen a
  searchIn = searchIn' (Attempts 1000) 10

  instance perturbLastEnum :: (Enum a, Arbitrary a) => Perturb (LastEnum a) where
    perturb n (LastEnum e) = LastEnum <$> cardPerturb1 (cardPerturb1F e n)

    dist (LastEnum a) (LastEnum b) = cardDist1 f a b where
      f (Cardinality sz) a b = if a == b then 0 else 1 / (2 * sz)

    dims e = lastEnumDims f e where
      f (Cardinality sz) e = if sz <= 0 then 0 else 1

  instance perturbNumber :: Perturb Number where
    perturb 0 n = pure n
    perturb d n = do
      u <- uniform -- 'up to' d
      s <- runSignum <$> arbitrary
      return $ s * (Math.exp(k0 * (u * d)) - 1) + n

    dist a b = 
      let from y = Math.log(y + 1) / k0
      in  (Math.min 1) <<< Math.abs <<< from $ Math.abs (a - b)

    dims = const 1

  instance perturbArray :: (Perturb a) => Perturb [a] where
    perturb d []  = pure $ []
    perturb 0 a   = sequence $ perturb 0 <$> a
    perturb d a   = let dx = delta (A.length a) d
                    in  sequence $ perturb dx <$> a
 
    dist a b = toDist $ A.zipWith dist a b

    dims = A.length

  instance perturbTuple :: (Perturb a, Perturb b) => Perturb (Tuple a b) where
    perturb d (Tuple a b) = let dx = delta 2 d
                            in  Tuple <$> (perturb dx a) <*> (perturb dx b)

    dist (Tuple a1 b1) (Tuple a2 b2) = toDist [dist a1 a2, dist b1 b2]

    dims (Tuple a b) = dims a + dims b

  instance perturbEnumTuple :: (Enum a, Enum b, Perturb b) => Perturb (Tuple a b) where
    perturb 0 t = pure t
    perturb d t = cardPerturb2 f where
      f (Cardinality sz1) (Cardinality sz2) = 
        let ds = dims t
            d1 = let f (Cardinality sz) a = if sz <= 0 then 0 else 1
                 in  (cardDims f (fst t))
            da = delta ds d
            db = (ds - d1) * da

        in do left  <- if da < 1 / (2 * sz1) then pure (fst t) else fromJust <<< toEnum <$> chooseInt 0 (sz1 - 1)
              right <- perturb db (snd t)
              return $ Tuple left right

    dist (Tuple a1 b1) (Tuple a2 b2) = cardDist2 f a1 a2 b1 b2 where
      f (Cardinality sz1) (Cardinality sz2) a1 a2 b1 b2 = toDist [if a1 == a2 then 0 else 1 / (2 * sz1), dist b1 b2]

    dims (Tuple a b) = 
      let f (Cardinality sz) a = if sz <= 0 then 0 else 1
      in  (cardDims f a) + dims b

  -- This instance must degrade to perturbing a single enum in the case the 
  -- cardinality of the second is 0. Which it does.
  instance perturbEnumEither :: (Enum a, Enum b, Perturb b) => Perturb (Either a b) where
    perturb 0 e = pure e
    perturb d e = cardPerturb2E f where
      f (Cardinality sz1) (Cardinality sz2) = 
        let tot  = sz1 + sz2 
            arbA = Left  <<< fromJust <<< toEnum
            arbB = Right <<< fromJust <<< toEnum

        in  if d < 1 / (2 * tot) then pure e
            else do which <- chooseInt 0 (tot - 1)
                    return $ if which < sz1 then arbA which
                             else arbB (which - sz1)

    dist e1 e2 = cardDist2E f e1 e2 where
      -- TODO: overlapping instances error for Either a b (???) in `e1 == e2`
      f (Cardinality sz1) (Cardinality sz2) (Left  l1) (Left  l2) = if l1 == l2 then 0 else 1 / (2 * (sz1 + sz2))
      f (Cardinality sz1) (Cardinality sz2) (Right r1) (Right r2) = if r1 == r2 then 0 else 1 / (2 * (sz1 + sz2))
      f (Cardinality sz1) (Cardinality sz2) _ _ = 1 / (2 * (sz1 + sz2))

    dims e = enumEitherDims f e where
      f (Cardinality sz1) (Cardinality sz2) e = case e of 
        Left  l -> if sz1 <= 0 then 0 else 1
        Right r -> dims r

  instance perturbArrayEnum :: (Enum a, Arbitrary a) => Perturb [a] where
    perturb d [] = pure $ []
    perturb 0 a  = pure $ a
    perturb d a  =  let len = A.length a
                    in do k    <- chooseInt 0 (d * len)             -- how many to modify?
                          idxs <- nChooseK k (A.range 0 (len - 1))  -- which ones to modify?
                          xs   <- vectorOf k arbitrary              -- pick anything for the modified
                          let updates = A.zipWith Tuple idxs xs
                          return $ foldl (flip \t -> A.updateAt (fst t) (snd t)) a updates

    dist a1 a2 = (sum $ zipped) / A.length zipped where
      f c1 c2 = if c1 == c2 then 0 else 1

      zipped = A.zipWith f a1 a2

    dims a = sum $ (dimsEnumArray f <$> a) where
      f (Cardinality sz) a = sz

  instance perturbString :: Perturb String where
    perturb d s = S.fromCharArray <$> perturb d (S.toCharArray s)

    dist s1 s2 = dist (S.toCharArray s1) (S.toCharArray s2)

    dims = dims <<< S.toCharArray

  -- magical constants
  maxNumber :: Number
  maxNumber = 9007199254740992

  -- math
  k0 :: Number
  k0 = Math.log(maxNumber + 1)

  square :: Number -> Number
  square = flip Math.pow 2

  toDist :: [Number] -> Number
  toDist xs = Math.sqrt (sum $ square <$> xs)

  delta :: Number -> Number -> Number
  delta n d = Math.sqrt (d * d / n)

  -- FIXME: this workaround is still required as of psc 0.5.6.3
  ifThenElse p a b = if p then a else b

  -- ScopedTypeVariables
  enumEitherDims :: forall a b. (Enum a, Enum b, Perturb b) => (Cardinality a -> Cardinality b -> Either a b -> Number) -> Either a b -> Number
  enumEitherDims f = f cardinality cardinality

  -- ScopedTypeVariables
  lastEnumDims :: forall a. (Enum a, Arbitrary a) => (Cardinality a -> LastEnum a -> Number) -> LastEnum a -> Number
  lastEnumDims f = f cardinality

  -- ScopedTypeVariables
  dimsEnumArray :: forall a b. (Enum a) => (Cardinality a -> a -> b) -> a -> b
  dimsEnumArray f = f cardinality

  -- ScopedTypeVariables
  cardPerturb1 :: forall f a. (Enum a) => (Cardinality a -> f a) -> f a
  cardPerturb1 f = f cardinality

  -- ScopedTypeVariables
  cardDist1 :: forall a. (Enum a) => (Cardinality a -> a -> a -> Number) -> a -> a -> Number
  cardDist1 f = f cardinality

  -- ScopedTypeVariables
  cardDims :: forall a. (Enum a) => (Cardinality a -> a -> Number) -> a -> Number
  cardDims f = f cardinality

  -- workaround to avoid:
  -- Attempted to unify a constrained type (Test.QuickCheck.Arbitrary u15286) => 
  -- Test.QuickCheck.LCG.Gen<u15286> with another type.
  cardPerturb1F :: forall a. (Arbitrary a) => a -> Number -> Cardinality a -> Gen a
  cardPerturb1F a n (Cardinality sz) = if n < 1 / (2 * sz) then pure a else arbitrary

  -- ScopedTypeVariables
  cardPerturb2 :: forall f a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> Gen (Tuple a b)) -> Gen (Tuple a b)
  cardPerturb2 f = f cardinality cardinality

  -- ScopedTypeVariables
  cardPerturb2E :: forall a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> Gen (Either a b)) -> Gen (Either a b)
  cardPerturb2E f = f cardinality cardinality

  -- ScopedTypeVariables
  cardDist2 :: forall a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> a -> a -> b -> b -> Number) -> a -> a -> b -> b -> Number
  cardDist2 f = f cardinality cardinality

  -- ScopedTypeVariables
  cardDist2E :: forall a b. (Enum a, Enum b) => (Cardinality a -> Cardinality b -> Either a b -> Either a b -> Number) -> Either a b -> Either a b -> Number
  cardDist2E f = f cardinality cardinality