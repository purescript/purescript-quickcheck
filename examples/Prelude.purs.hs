module PreludeTests where

import Prelude
import Control.Monad.Eff
import Test.QuickCheck
import Data.Eq
import Debug.Trace

testConst :: Number -> Number -> Number -> Boolean
testConst a b c = const a b == const a c

main = do
  Debug.Trace.trace "testConst:"
  quickCheck testConst

  Debug.Trace.trace "Precedence of && and ||:"
  quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))
  
  Debug.Trace.trace "Test Eq instance for Ref:"
  quickCheck $ \a -> (Ref a :: Ref Number) == Ref a
  quickCheck $ \a -> not $ (Ref a :: Ref Number /= Ref a)
