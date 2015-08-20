-- | Generic deriving for `Arbitrary` and `Coarbitrary` instances.

module Test.QuickCheck.Generic 
  ( gArbitrary
  , gCoarbitrary
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Array (uncons)
import Data.Traversable (traverse)
import Data.Generic
import Data.Monoid.Endo
import Data.Foldable (Foldable, foldMap)

import Control.Lazy (defer)

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

-- | Generate arbitrary values for any `Generic` data structure.
-- |
-- | _Note_: this function _is_ partial, since it does not handle ADTs with zero constructors
-- | such as `Void`.
gArbitrary :: forall a. (Generic a) => Gen a
gArbitrary = unsafeFromSpine <$> go (toSignature (anyProxy :: Proxy a))
  where
  unsafeFromSpine :: GenericSpine -> a
  unsafeFromSpine = fromJust <<< fromSpine
  
  go :: GenericSignature -> Gen GenericSpine
  go (SigProd ctors) = 
    case uncons (map toGen ctors) of
      Just cons -> oneOf cons.head cons.tail
    where
    toGen :: { sigConstructor :: String, sigValues :: Array (Unit -> GenericSignature) } -> Gen GenericSpine
    toGen ctor = defer \_ -> map (SProd ctor.sigConstructor) (traverse (\f -> const <$> go (f unit)) ctor.sigValues)
  go (SigRecord fields) = SRecord <$> traverse toField fields
    where
    toField :: { recLabel :: String, recValue :: Unit -> GenericSignature } ->
           Gen { recLabel :: String, recValue :: Unit -> GenericSpine }
    toField o = defer \_ -> map ({ recLabel: o.recLabel, recValue: _ } <<< const) (go (o.recValue unit))
  go SigNumber    = map SNumber  arbitrary
  go SigBoolean   = map SBoolean arbitrary
  go SigInt       = map SInt     arbitrary
  go SigString    = map SString  arbitrary
  go (SigArray f) = map (SArray <<< map const) (arrayOf (go (f unit)))

-- | Perturb a generator using a `Generic` data structure.
gCoarbitrary :: forall a r. (Generic a) => a -> Gen r -> Gen r
gCoarbitrary a = go (toSpine a)
  where
  go :: GenericSpine -> Gen r -> Gen r    
  go (SArray ss) = applyAll (map (go <<< ($ unit)) ss)
  go (SString s) = coarbitrary s
  go (SInt i)    = coarbitrary i
  go (SNumber n) = coarbitrary n
  go (SRecord fs) = applyAll (map (\f -> coarbitrary f.recLabel <<< go (f.recValue unit)) fs) 
  go (SProd ctor ss) = coarbitrary ctor <<< applyAll (map (go <<< ($ unit)) ss)
  
  applyAll :: forall f a. (Foldable f) => f (a -> a) -> a -> a
  applyAll = runEndo <<< foldMap Endo