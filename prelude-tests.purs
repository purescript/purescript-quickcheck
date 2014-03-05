module Main where

import Prelude
import Control.Monad.Eff
import QuickCheck
import Data.Eq
import Debug.Trace

testConst :: Number -> Number -> Number -> Boolean
testConst a b c = const a b == const a c

testReadShow :: Number -> Boolean
testReadShow a = a == read (show a)

main = do
  trace "testConst:"
  quickCheck testConst

  trace "testReadShow:"
  quickCheck testReadShow

  trace "Precedence of && and ||:"
  quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))
  
  trace "Test Eq instance for Ref:"
  quickCheck $ \a -> (Ref a :: Ref Number) == Ref a
  quickCheck $ \a -> not $ (Ref a :: Ref Number /= Ref a)
