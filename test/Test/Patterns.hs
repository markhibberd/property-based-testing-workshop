{-# LANGUAGE TemplateHaskell #-}
module Test.Patterns where

import           Data.List (sort)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Patterns (slidingAll)

import qualified Test.SimpleGen as SimpleGen


--
-- == Module: Patterns ==
--
-- The same patterns come up repeatedly when finding good properties
-- to test in your code. Lets have a look at some of the most common.
--


-- == Exercise 6 == --

--
-- == Pattern: Round Trip ==
--
-- The 'Round Trip': There and back again.
--
-- This is a very common pattern of applying a transformation and
-- reversing it, this picks up inconsistencies and many unexpected
-- behaviours. This pattern works on very simple code like there
-- below that prints and then parses an number, through to very
-- complicated systems with state and subtle interactions.
--
-- Example:
--
-- > Just n === readMaybe (show n)
--

-- |
-- There are standard list functions 'unlines' for taking a
-- list of strings and converting it into a single string
-- separated by newlines, and 'lines' for taking a string
-- and splitting it on newline back into individual lines.
--
-- > unlines :: [String] -> String
-- > lines :: String -> [String]
--
-- This is a good candidate for a round trip test, your will often see
-- this 'symmetry' in well build programs and libraries.
--
prop_round_trip_lines_unlines :: Property
prop_round_trip_lines_unlines =
  property $ do
    strings <- forAll $ SimpleGen.list (Gen.filter (not . elem '\n') $ Gen.string (Range.linear 0 100) Gen.unicode)
    lines (unlines strings) === strings


-- == Exercise 7 == --

--
-- == Pattern: Idempotence ==
--
-- The 'Idempotent': Operations that can be applied repeatedly for
-- the same behaviour.
--
-- This is great for code that should have the same result for
-- various imputs. Good examples maybe sorting a list, or
-- lowercasing a string.
--

-- |
-- The standard list function 'sort', takes a list of elements
-- that can be compared and produces a sorted list.
--
-- > sort :: Ord a => [a] -> [a]
--
-- For the property we can test with 'Int' for a concrete case.
--
-- > sort :: [Int] -> [Int]
--
-- Verifying for 'Int' will verify for all types because of
-- the parametricity, this is a good example of types and
-- tests working together.
--
-- As a starting point, lets check that sorting a list
-- twice is the same as sorting it once.
--
prop_idempotent :: Property
prop_idempotent =
  property $ do
    unsorted <- forAll $ SimpleGen.list SimpleGen.int
    sort unsorted === sort (sort unsorted)

-- == Exercise 8 == --

--
-- == Pattern: Verification ==
--
-- The 'Verifier': hard to prove, easy to verify.
--
-- The verification pattern, is useful when it is easy to
-- produce a 'validation' function that determines if the
-- output is correct.
--

-- |
-- To improve our coverage of 'sort', lets use the
-- verification pattern.
--
-- To assist with this, there are functions in scope:
--
-- > slidingAll :: (Maybe a -> a -> Bool) -> [a] -> Bool
--
-- 'slidingAll' determines if all elements in a list satisfy a
-- predicate. 'slidingAll' works like the standard library 'all'
-- function except that it also provides you access to the 'previous'
-- value in the list (if there was one).
--
prop_verification :: Property
prop_verification =
  property $ do
    unsorted <- forAll $ SimpleGen.list SimpleGen.int
    assert $ slidingAll (\previous el -> case previous of
      Nothing ->
        True
      Just p ->
        p <= el) (sort unsorted)


-- == Exercise 9 == --

--
-- == Pattern: Invariants ==
--
-- The 'invariant': for things that are always true.
--
-- This is very powerful for stating simple things about complex
-- applications, an example might be that a shopping cart should never
-- have a negative total. A simple example would be that the length of
-- a list before and after calling map should always the same.
--

--
-- Again, we can improve our coverage of 'sort' with some invariants.
--
-- Our types guarantee all elements in the output come from
-- the input, our previous properties tell us that what is
-- in the output is sorted, but we haven't yet guaranteed that
-- all of the inputs are in the output. We can show this with
-- two invariants:
--  - Every input appears in the output.
--  - The length of the input is the same as the output (to cover
--    duplicate elements).
--

-- |
-- Verify that the length of the sorted list is always the same
-- as the input list.
--
prop_invariant_length :: Property
prop_invariant_length =
  property $ do
    unsorted <- forAll $ SimpleGen.list SimpleGen.int
    length unsorted === length (sort unsorted)


-- |
-- Verify that all input elements appear in the output.
--
-- Functions that will help:
--
-- 'all' checks a predicate against every element in a list.
--
-- > all :: (a -> Bool) -> [a] -> Bool
--
-- 'elem' checks an element exists in a list.
--
-- > elem :: a -> [a] -> Bool
--
prop_invariant_input_maintained :: Property
prop_invariant_input_maintained =
  property $ do
    unsorted <- forAll $ SimpleGen.list SimpleGen.int
    let sorted = sort unsorted
    assert $ all (\el -> elem el sorted) unsorted


-- == Exercise 10 ==

--
-- == Pattern: Different Paths ==
--
-- The 'Different Path': Operations that can be applied in
-- different orders but should achieve the same result.
--

-- |
-- A simple example of this could be to test the 'sum' function.
--
-- In words we want to say: The 'sum' of the concatenation of two
-- lists is the same as 'sum' of the individual lists added together.
--
-- For the purpose of this property you can think about
-- sum having the type below:
--
-- > sum :: [Int] -> Int
--
-- To concatenate two lists:
--
-- > (++) :: [a] -> [a] -> [a]
--
-- > [1, 2, 3] ++ [4, 5, 6] == [1, 2, 3, 4, 5, 6]
--
prop_sum :: Property
prop_sum =
  property $ do
    a <- forAll $ SimpleGen.list SimpleGen.int
    b <- forAll $ SimpleGen.list SimpleGen.int
    sum (a ++ b) === (sum a + sum b)


-- |
-- This sum property is a good example of multiple paths, but
-- there are some cases where this is even more effective. An
-- example of this is 'reverse'. Using the different paths
-- approach with 'reverse' means there are very few bugs that
-- could ever get through.
--
-- > reverse :: [a] -> [a]
--
prop_reverse :: Property
prop_reverse =
  property $ do
    a <- forAll $ SimpleGen.list SimpleGen.int
    b <- forAll $ SimpleGen.list SimpleGen.int
    reverse (a ++ b) === (reverse b ++ reverse a)


-- == Exercise 11 ==

--
-- == Pattern: Reconstruction ==
--
-- The 'reconstruction': building inputs to create
-- known answers.
--
-- This is something that you have probably already seen,
-- but is a simple but very powerful technique and possibly
-- the most useful technique to understand.
--
-- An early problem that people often run into when
-- starting property based testing is, they can think
-- of things where random inputs work, and they can
-- think of hard-coded cases, but no middle ground.
--
-- Reconstruction lets us know something about our
-- input without caring about the other details.
--

-- |
-- Lets test filter with reconstruction.
--
-- We want to reconstruct a list where we know that
-- it has at least one element, that element only
-- occurs once in the list.
--
-- Using this reconstructed list we can filter where
-- the predicate matches that element.
--
-- This should result in a single element list with
-- our known element.
--
prop_reconstruction :: Property
prop_reconstruction =
  property $ do
    known <- forAll $ SimpleGen.int
    lefts <- forAll $
      SimpleGen.list (Gen.filter (/= known) SimpleGen.int)
    rights <- forAll $
      SimpleGen.list (Gen.filter (/= known) SimpleGen.int)
    filter (== known) (lefts ++ [known] ++ rights) === [known]


-- == Exercise 12 ==

--
-- == Pattern: Induction ==
--
-- The 'Induction': Testing for base cases and then
-- general cases, as per the style of mathematical
-- induction.
--
-- This is a pattern you have already seen in the
-- basic exercises, so there will be a little less
-- help here.
--
-- Produce 3 properties, a base case, a singleton case,
-- and a general case to test the map function.
--
-- > map :: (a -> b) -> [a] -> [b]
--


-- == Informational ==

--
-- There are no exercises for these patterns, but they are
-- useful to know, and are often used when dealing with
-- complex code.
--

--
-- == Pattern: Oracle ==
--
-- The 'Oracle': A simplified model of the correct behaviour.
--
-- This is great for things like performance critical code,
-- testing optimised code against simpler easier to understand
-- code. Or complex distributed systems, testing a hadoop
-- map-reduce operation against an in-memory list implementation.
--


tests :: IO Bool
tests =
  checkParallel $$(discover)
