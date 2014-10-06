module PreludeTests where

import Control.Monad.Eff
import Data.Eq
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.LCG
import Control.Monad.Trampoline
import Data.Monoid
import Data.Tuple
import Data.Maybe
import Data.Foldable
import qualified Data.Array as Array
import qualified Math as Math

between :: forall a. (Ord a) => a -> a -> a -> Boolean
between min max = \n -> n >= min && n <= max

data Mega  = Mega { 
  arrayOf     :: [Number],
  arrayOf1    :: [Number],
  choose      :: Number,
  chooseInt   :: Number,
  collectAll  :: [Number],
  allInArray  :: [Number],
  allInRange  :: [Number],
  dropGen     :: [Number],
  takeGen     :: [Number],
  elements    :: [String],
  extend      :: [String],
  infinite    :: [String] }

{- TODO: Remaining cases
  , frequency 
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
  , unfoldGen
  , uniform 
  , variant 
  , vectorOf 
  -}  

data OneToTen = OneToTen Number

runOneToTen :: OneToTen -> Number
runOneToTen (OneToTen n) = n

instance arbOneToTen :: Arbitrary OneToTen where
  arbitrary = OneToTen <$> chooseInt 0 10

instance arbMega :: Arbitrary Mega where
  arbitrary = do 
    arrayOf'    <- arrayOf (choose 0 10)
    arrayOf1'   <- arrayOf1 (choose 0 10)
    choose'     <- choose 0 10
    chooseInt'  <- chooseInt 0 10
    collectAll' <- collectAll mempty (allInArray [0, 1, 2])
    allInArray' <- collectAll mempty (allInArray [0, 1, 2])
    allInRange' <- collectAll mempty (allInRange 0 10)
    dropGen'    <- collectAll mempty $ dropGen 2 (allInArray [2, 1, -1])
    takeGen'    <- collectAll mempty $ takeGen 2 (allInArray [2, 1, -1])
    elements'   <- arrayOf $ elements "foo" ["bar", "baz"]
    extend'     <- collectAll mempty $ extend 3 (pure "5")
    infinite'   <- collectAll mempty $ takeGen 4 (infinite $ pure "foo")
    return $ Mega { 
      arrayOf:    arrayOf', 
      arrayOf1:   (case arrayOf1' of Tuple a as -> a : as), 
      choose:     choose',  
      chooseInt:  chooseInt', 
      collectAll: collectAll', 
      allInArray: allInArray', 
      allInRange: allInRange',
      dropGen:    dropGen',
      takeGen:    takeGen',
      elements:   elements',
      extend:     extend',
      infinite:   infinite' }

verify_gen :: Mega -> Result
verify_gen (Mega m) = fold [
  all (between 0 10) m.arrayOf                        <?> "arrayOf: "     ++ show m.arrayOf,
  Array.length m.arrayOf1 >= 1                        <?> "arrayOf1: "    ++ show m.arrayOf1,
  between 0 10 m.choose                               <?> "choose: "      ++ show m.choose,
  between 0 10 m.chooseInt && 
  Math.floor(m.chooseInt) == m.chooseInt              <?> "chooseInt: "   ++ show m.chooseInt,
  m.collectAll == [0, 1, 2]                           <?> "collectAll: "  ++ show m.collectAll,
  m.allInArray == [0, 1, 2]                           <?> "allInArray: "  ++ show m.allInArray,
  m.allInRange == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  <?> "allInRange: "  ++ show m.allInRange,
  m.dropGen == [-1]                                   <?> "dropGen: "     ++ show m.dropGen,
  m.takeGen == [2, 1]                                 <?> "takeGen: "     ++ show m.takeGen,
  all (flip elem ["foo", "bar", "baz"]) m.elements    <?> "elements: "    ++ show m.elements,
  m.extend == ["5", "5", "5"]                         <?> "extend: "      ++ show m.extend,
  m.infinite == ["foo", "foo", "foo", "foo"]          <?> "infinite: "    ++ show m.infinite ]
  
main = do
  trace "Gen combinators"
  quickCheck $ verify_gen

  trace "foldGen"
  quickCheck $ (runTrampoline $ foldGen (\a b -> Just $ a + b) 1 mempty (allInArray [1, 2, 3])) == 7

  trace "Fair distribution of booleans"
  statCheck (1/2) $ (==) true
  
  trace "Fair distribution of ints"
  statCheck (1/11) $ runOneToTen >>> ((==) 1)

