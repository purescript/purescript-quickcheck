module Test.QuickCheck.Functions where

import Control.Monad.Eff.Random.Extras.Unsafe (choice)
import Test.QuickCheck (Arb)
import qualified Data.String as S

-- |
-- Numbers
--

instance arbNumToNum :: Arb (Number -> Number) where
  arb = choice [ \x -> x
               , \x -> x + 1
               , \x -> x - 1
               , \x -> x * x
               , \x -> -x ]
               
instance showNumToNum :: Show (Number -> Number) where
  show _ = "<Number -> Number>"
  
-- |
-- Strings
--
               
instance arbStrToStr :: Arb (String -> String) where
  arb = choice [id, S.toUpper, S.toLower]
  
instance showStrToStr :: Show (String -> String) where
  show _ = "<String -> String>"
  
-- |
-- Booleans
--
               
instance arbBoolToBool :: Arb (Boolean -> Boolean) where
  arb = choice [id, not]

instance showBoolToBool :: Show (Boolean -> Boolean) where
  show _ = "<Boolean -> Boolean>"
  
-- |
-- Conversions to String
--
               
instance arbNumToStr :: Arb (Number -> String) where
  arb = return show
  
instance showNumToStr :: Show (Number -> String) where
  show _ = "<Number -> String>"
  
instance arbBoolToStr :: Arb (Boolean -> String) where
  arb = return show
  
instance showBoolToStr :: Show (Boolean -> String) where
  show _ = "<Boolean -> String>"
  
-- |
-- Conversions to Boolean
--
  
instance arbNumToBool :: Arb (Number -> Boolean) where
  arb = return $ \x -> (x % 2) == 0
  
instance showNumToBool :: Show (Number -> Boolean) where
  show _ = "<Number -> Boolean>"
  
instance arbStrToBool :: Arb (String -> Boolean) where
  arb = return $ \x -> (S.length x) > 0
               
instance showStrToBool :: Show (String -> Boolean) where
  show _ = "<String -> Boolean>"
  
-- |
-- Conversions to Number
--
  
instance arbStrToNum :: Arb (String -> Number) where
  arb = return S.length
               
instance showStrToNum :: Show (String -> Number) where
  show _ = "<String -> Number>"
  
instance arbBoolToNum :: Arb (Boolean -> Number) where
  arb = return $ \x -> if x then 1 else 0
               
instance showBoolToNum :: Show (Boolean -> Number) where
  show _ = "<Boolean -> Number>"
  

