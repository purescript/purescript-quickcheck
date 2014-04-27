module PreludeTests where

import Control.Monad.Eff
import Data.Eq
import Debug.Trace
import Test.QuickCheck

testConst :: Number -> Number -> Number -> Boolean
testConst a b c = const a b == const a c

mkMessage :: (Number -> Number) -> String
mkMessage f = "Test failed for function (" 
  ++ show (f 0) ++ ", " 
  ++ show (f 1) ++ ", " 
  ++ show (f 2) ++ ")"

main = do
  Debug.Trace.trace "testConst:"
  quickCheck testConst

  Debug.Trace.trace "id is a left unit for <<<"
  quickCheck $ \f a -> ((id <<< f) (a :: Number) == (f a) :: Number) <?> mkMessage f

  Debug.Trace.trace "Precedence of && and ||:"
  quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))
  
  Debug.Trace.trace "Test Eq instance for Ref:"
  quickCheck $ \a -> (Ref a :: Ref Number) == Ref a
  quickCheck $ \a -> not $ (Ref a :: Ref Number /= Ref a)

