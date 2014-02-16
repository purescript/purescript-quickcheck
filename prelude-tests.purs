module Main where

import Prelude
import Eff
import QuickCheck

testConst :: Number -> Number -> Number -> Boolean
testConst a b c = const a b == const a c

testReadShow :: Number -> Boolean
testReadShow a = a == read (show a)

main = do
  Trace.trace "testConst:"
  quickCheck testConst

  Trace.trace "testReadShow:"
  quickCheck testReadShow

  Trace.trace "Precedence of && and ||:"
  quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))
  
  Trace.trace "Test Eq instance for Ref:"
  quickCheck $ \a -> (Ref a :: Ref Number) == Ref a
  quickCheck $ \a -> not $ (Ref a :: Ref Number /= Ref a)
