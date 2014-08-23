module PreludeTests where

import Control.Monad.Eff
import Data.Eq
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.LCG

testConst :: Number -> Number -> Number -> Boolean
testConst a b c = const a b == const a c

mkMessage :: (Number -> Number) -> String
mkMessage f = "Test failed for function ("
  ++ show (f 0) ++ ", "
  ++ show (f 1) ++ ", "
  ++ show (f 2) ++ ")"

data Tree = Leaf Number | Branch Tree Tree

instance arbTree :: Arbitrary Tree where
  arbitrary = sized tree
    where
      tree n | n <= 10730000 = Leaf <$> uniform
      -- tree n          = Leaf <$> uniform
      tree n          = Branch <$> tree (n / 200000) <*> tree (n / 200000)

isLeaf :: Tree -> Boolean
isLeaf (Leaf _) = true
isLeaf _        = false

main = do
  -- Debug.Trace.trace "testConst:"
  -- quickCheck testConst

  -- Debug.Trace.trace "id is a left unit for <<<"
  -- quickCheck $ \f a -> ((id <<< f) (a :: Number) == (f a) :: Number) <?> mkMessage f

  -- Debug.Trace.trace "Precedence of && and ||:"
  -- quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))

  -- Debug.Trace.trace "Test Eq instance for Ref:"
  -- quickCheck $ \a -> (Ref a :: Ref Number) == Ref a
  -- quickCheck $ \a -> not $ (Ref a :: Ref Number /= Ref a)

  quickCheck' 5000 $ forAll (arbitrary :: Gen Tree) (\t -> if isLeaf t then Failed "Leaf" else Success)
