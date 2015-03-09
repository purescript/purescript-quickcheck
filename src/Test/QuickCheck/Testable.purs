module Test.QuickCheck.Testable where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Result = Success | Failed String

instance showResult :: Show Result where
  show Success = "Success"
  show (Failed msg) = "Failed: " ++ msg

class Testable prop where
  test :: prop -> Gen Result

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arbitrary
    test (f t)
