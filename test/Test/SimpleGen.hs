module Test.SimpleGen (
    int
  , list
  ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


int :: Gen Int
int =
  Gen.int (Range.linearFrom 0 (-100) 100)

list :: Gen a -> Gen [a]
list gen =
  Gen.list (Range.linear 0 100) gen
