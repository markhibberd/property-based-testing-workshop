{-# LANGUAGE TemplateHaskell #-}
module Test.Basics where

import qualified Basics

import           Control.Monad (when)

import           Hedgehog

import qualified Test.SimpleGen as SimpleGen

-- |
-- Welcome to property based testing.
--
-- We first have to work through the basics, to do this we will be
-- testing some really simple functions. The goal of these first
-- exercises is to introduce the concepts of property based testing
-- and the functions provided by the library we are using to help us.
--
-- Once we work through the basics, the next exercises will start
-- to
--
prop_introduction :: Property
prop_introduction =
  -- First, lets get some syntax out of the way.

  -- The 'property' function applies the standard configuration to a
  -- property test. For now, all of our tests will start like this.
  property $ do

    -- If you want some help debugging a failing test you can:

    annotate "This is a way to display a message when a test fails"
    annotateShow (Just 'a')

    footnote "Display something at the end of the failure report"
    footnoteShow (Just 'b')

    -- If we want to have a value in our tests we use forAll and a generator:

    n <- forAll SimpleGen.int
    m <- forAll SimpleGen.int
    ns <- forAll $ SimpleGen.list SimpleGen.int

    -- Our tests will want to assert things like this:
    success
    when (n /= n) $
      failure
    n === n
    n + 1 /== n
    assert (n >= m || m > n)
    assert (length ns == 0 || length ns > 0)

--
-- Running tests.
--
-- To run one test:
--
-- >  Hedgehog.check Test.Basics.prop_introduction
--
-- To run all the tests in a module.
--
-- >  Test.Basics.tests
--
-- To run all the tests.
--
-- >  Main.main
--
-- To sample the output of a generator:
--
-- > Hedgehog.Gen.sample Test.SimpleGen.int
--
-- If these don't work for you, now is the time to ask for help, being
-- able easily to run the tests will help you a lot as you work
-- through the problems.
--


-- == Demonstration 1 == --

-- |
-- Access the first element of a list, will return 'Just'
-- if the list has at least one element, will return Nothing
-- if the list is empty.
--
safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    [] ->
      Nothing
    (x:_) ->
      Just x

-- |
-- Check that the empty list always returns nothing.
--
-- /note:/ We have to have to specify the type of the list so that the
-- right comparison is used for equals.
--
prop_safeHead_empty :: Property
prop_safeHead_empty =
  property $
    safeHead ([] :: [Int]) === Nothing

-- |
-- Check that for any non-empty list, the first element is always returned.
--
prop_safeHead_nonEmpty :: Property
prop_safeHead_nonEmpty =
  property $ do
    x <- forAll $ SimpleGen.int
    xs <- forAll $ SimpleGen.list SimpleGen.int
    safeHead (x:xs) === Just x



-- == Exercise 1 == --

--
-- We have a function 'headOr', that takes a default value
-- and a list, returning the first element of the list if
-- the list is non-empty, or the default value if the element
-- is empty.
--
-- > headOr :: a -> [a] -> a -> a
--


-- |
-- Check that we get the default value when the list is empty.
--
prop_headOr_empty :: Property
prop_headOr_empty =
  property $ do
    dfault <- forAll SimpleGen.int
    headOr dfault [] === dfault

-- |
-- Check that we get the first value in the list when the list is non-empty.
--
prop_headOr_non_empty :: Property
prop_headOr_non_empty =
  property $ do
    dfault <- forAll SimpleGen.int
    x <- forAll SimpleGen.int
    xs <- forAll $ SimpleGen.list SimpleGen.int
    headOr dfault (x:xs) === x

-- == Exercise 1: Redux == --

--
-- The current implementation of headOr works as specified, but how do
-- your tests go against some buggy code, try your tests against the
-- other implementations shown in the comments.
--
headOr :: a -> [a] -> a
headOr =
  Basics.headOr'
--  Basics.headOr''


-- == Exercise 2 == --

--
-- We have a function 'total', that takes a list of integers and
-- produces the sum of all of the elements in the list:
--
-- > total :: [Int] -> Int
--
--

-- |
-- Check that the empty list always returns 0.
--
prop_total_empty :: Property
prop_total_empty =
  property $ do
    total ([] :: [Int]) === 0

-- |
-- Check that the single element list always returns the single element.
--
prop_total_singleton :: Property
prop_total_singleton =
  property $ do
    x <- forAll $ SimpleGen.int
    total [x] === x

-- |
-- Check that larger lists always return the total of all of their elements.
--
prop_total_larger :: Property
prop_total_larger =
  property $ do
    x <- forAll $ SimpleGen.int
    xs <- forAll $ SimpleGen.list SimpleGen.int
    total (x:xs) === x + total xs

-- == Exercise 2: Redux == --

--
-- The current implementation of total works as specified, but how do
-- your tests go against some buggy code, try your tests against the
-- other implementations shown in the comments.
--
total :: [Int] -> Int
total =
  Basics.total'
--  Basics.total''
--  Basics.total'''


-- == Exercise 3 == --

--
-- We have a function 'size', that takes a list of integers and
-- produces the length of of the the list:
--
-- > size :: [Int] -> Int
--
--

--
-- Define your own properties.
--
-- /hint/: Think in cases, empty list, singleton list, larger lists.
--


-- == Exercise 3: Redux == --


--
-- The current implementation of size works as specified, but how do
-- your tests go against some buggy code, try your tests against the
-- other implementations shown in the comments.
--
size :: [Int] -> Int
size =
  Basics.size'
--  Basics.size''


tests :: IO Bool
tests =
  checkParallel $$(discover)
