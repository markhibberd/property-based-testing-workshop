{-# LANGUAGE TemplateHaskell #-}
module Test.Practice where

import qualified Practice

import           Hedgehog

import qualified Test.SimpleGen as SimpleGen

-- == Exercise 13 ==

--
-- Filter takes a list and a predicate and returns a list
-- of the elements that satisfy the predicate.
--
-- > filter :: (a -> Bool) -> a -> [a]
--
-- Construct 5 properties to cover the filter function:
--
--  * The always true predicate.
--  * The always false predicate.
--  * Filter to a single value, ensure only that value occurs.
--  * Filter to everything but a single value, ensure the value
--    does not occur.
--  * Filter to everything but a unique single value, make sure
--    all other values appear
--

-- WORKSHOP EXERCISE


-- == Exercise 14 ==

--
-- Flatten takes a list of lists, and produces a single list.
--
-- > flatten :: [[a]] -> [a]
--
-- Construct properties to cover the flatten function.
--
-- /hint/: Use length as an invariant.
--

-- WORKSHOP EXERCISE


-- == Exercise 15 ==

--
-- Try to construct a 'buggy' version of flatten that passes
-- your properties.
--
-- Did it help you think of any more cases?
--

flatten2 :: [[a]] -> [a]
flatten2 =
  -- WORKSHOP EXERCISE
  error "todo"


-- == Exercise 16 ==

--
-- The permutations function returns the list of all permutations of the argument.
--
-- > permutations :: [a] -> [[a]]
--
-- Example:
--
-- >>> permutations "abc"
-- ["abc","bac","cba","bca","cab","acb"]
--
--
-- /hint/: There are two really good invariants.
-- /hint/: What is a valid permutation?
--
-- Some useful functions:
--
-- > factorial :: Int -> Int -- Factorial of an integer.
-- > nub :: Eq => [a] -> [a] -- Remove duplicates from list.
--

-- WORKSHOP EXERCISE


-- == Exercise 17 ==

--
-- The transpose function transposes the rows and columns of its argument.
--
-- > transpose :: [[a]] -> [[a]]
--
-- Example:
--
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
--
-- If some of the rows are shorter than the following rows, their elements are skipped:
--
-- >>> transpose [[10,11],[20],[],[30,31,32]]
-- [[10,20,30],[11,31],[32]]
--
-- /hint/: Round-trip for equal size rows.
-- /hint/: Reconstruction for mismatched rows.
--

-- WORKSHOP EXERCISE


tests :: IO Bool
tests =
  checkParallel $$(discover)
