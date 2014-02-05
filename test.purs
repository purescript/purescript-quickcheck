module Main where

import Prelude
import Eff
import QuickCheck

prop1 :: Number -> Number -> Boolean
prop1 a b = a < b

prop2 :: Number -> Number -> Boolean
prop2 a b = (a + b) == (b + a)

main = do
  quickCheck prop1
  quickCheck prop2
