module Test.QuickCheck.Classes where

import Test.QuickCheck (QC(..), Arb, quickCheck)

-- |
-- Functor
--

checkFunctor :: forall f a. (Functor f, Arb a, Arb (a -> a), Arb (f a), Eq (f a)) => f a -> QC
checkFunctor t = do
  quickCheck $ identity t
  quickCheck $ associativity t
  
  where
  
  identity :: forall f a. (Functor f, Arb a, Eq (f a)) => f a -> f a -> Boolean
  identity _ f = id <$> f == id f

  associativity :: forall f a. (Functor f, Arb a, Eq (f a)) => f a -> f a -> (a -> a) -> (a -> a) -> Boolean
  associativity _ f p q = (p <<< q) <$> f == ((<$>) p <<< (<$>) q) f

-- |
-- Applicative
--

checkApplicative :: forall f a b c. (Applicative f, Arb (f a), Arb (f (a -> b)), Arb (f (b -> c)), Arb (a -> b), Arb a, Eq (f a), Eq (f b), Eq (f c)) => f a -> f b -> f c -> QC
checkApplicative ta tb tc = do
  quickCheck $ identity ta
  quickCheck $ composition ta tb tc
  quickCheck $ homomorphism ta tb
  quickCheck $ interchange ta tb

  where
  
  identity :: forall f a. (Applicative f, Arb (f a), Eq (f a)) => f a -> f a -> Boolean
  identity _ v = (pure id <*> v) == v

  composition :: forall f a b c. (Applicative f, Arb (f (b -> c)), Arb (f (a -> b)), Arb (f a), Eq (f c)) => f a -> f b -> f c -> f (b -> c) -> f (a -> b) -> f a -> Boolean
  composition _ _ _ u v w = (pure (<<<) <*> u <*> v <*> w) == (u <*> (v <*> w))

  homomorphism :: forall f a b. (Applicative f, Arb (a -> b), Arb a, Eq (f b)) => f a -> f b -> (a -> b) -> a -> Boolean
  homomorphism _ tb f x = (pure f <*> pure x) == (pure (f x) `asTypeOf` tb)

  interchange :: forall f a b. (Applicative f, Arb a, Arb (f (a -> b)), Eq (f b)) => f a -> f b -> a -> f (a -> b) -> Boolean
  interchange _ _ y u = (u <*> pure y) == (pure (\x -> x y) <*> u)

-- |
-- Monad
--

checkMonad :: forall m a. (Monad m, Arb a, Arb (a -> m a), Arb (m a), Eq (m a)) => m a -> QC
checkMonad t = do
  quickCheck $ leftIdentity t
  quickCheck $ rightIdentity t
  quickCheck $ associativity t
  
  where
  
  leftIdentity :: forall m a. (Monad m, Arb a, Eq (m a)) => m a -> a -> (a -> m a) -> Boolean
  leftIdentity _ x f = (return x >>= f) == (f x)

  rightIdentity :: forall m a. (Monad m, Arb a, Eq (m a)) => m a -> m a -> Boolean
  rightIdentity _ m = (m >>= return) == m

  associativity :: forall m a. (Monad m, Arb a, Eq (m a)) => m a -> m a -> (a -> m a) -> (a -> m a) -> Boolean
  associativity _ m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
