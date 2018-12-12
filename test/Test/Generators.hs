{-# LANGUAGE TemplateHaskell #-}
module Test.Generators where

import qualified Data.List as List

import           Generators (Address (..))
import qualified Generators

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Test.SimpleGen as SimpleGen

--
-- So far we have been using very simple generators, but hopefully you
-- will have seen how well they can both find bugs and also help you
-- narrow down the bugs to specific cases.
--
-- The next step in understanding and using property based testing is
-- being able to construct your own generators or use more complex
-- generators to make the same properties more useful.
--

-- == Demonstration 2 == --

-- |
--
-- You have already seen some simplified generators for integers.
--
-- These are simplified by defaulting to int values between -100 and 100.
--
prop_simple_gen_int :: Property
prop_simple_gen_int =
  property $ do
    int <- forAll SimpleGen.int
    assert $ int >= (-100) && int <= 100


-- |
-- You have already seen seen a parameterised generator for lists.
--
-- These are simplified by a list of between 0 and 100 elements
-- using the provided generator for the elements.
--
prop_simple_gen_list :: Property
prop_simple_gen_list =
  property $ do
    list <- forAll $ SimpleGen.list SimpleGen.int
    assert $ length list >= 0 && length list <= 100


-- |
-- We can use the Range API to be more specific about our
-- generators. Linear means that as we increase the number of
-- tests we run, the number will tend to get bigger.  Here '10' is
-- the 'origin', that is the value we shrink towards, shrinking is
-- important to debugging. An alternative to a generator that is
-- 'linear' to the size (i.e. test iteration) of the test, we
-- could use 'Range.constant' to ignore the size, or use
-- 'Range.exponential' to create an exponential scale.
--
prop_int_range_linear :: Property
prop_int_range_linear =
  property $ do
    smallint <- forAll $ Gen.int (Range.linear 10 20)
    assert $ smallint >= 10 && smallint <= 20

-- |
-- Using the '*From' variants of the 'Range' API we can
-- control how properties shrink for debugging. The first
-- argument is the origin, if you experiment with changing
-- it, and switching to the failing test you will find that
-- the property always shrinks towards the origin.
--
--
-- == EXPERIMENT ==
--
-- Uncomment the 'failure', and see that the test will always shrink
-- towards the origin of 0.
--
-- Try changing 0 to 100, or 50, and see what happens.
--
-- This is how generators help narrow our bugs to the smallest
-- possible case.
--
prop_int_range_linear_from :: Property
prop_int_range_linear_from =
  property $ do
    shrinking <- forAll $ Gen.int (Range.linearFrom 0 (-100) 100)
    assert $ shrinking == shrinking
--    failure


-- |
-- The same 'Range' api can be used to control all types of
-- generators, such as our list generator. Here we are generating
-- a list of 0 - 10 elements, each an int between 0 and 10.
--
prop_list_range_linear :: Property
prop_list_range_linear =
  property $ do
    smalllist <- forAll $
      Gen.list (Range.linear 0 10) $
        Gen.int (Range.linear 0 10)
    assert $ sum smalllist <= (10 * 10)

-- |
-- A singleton range is useful for generating lists of a specific length.
--
prop_list_range_constant :: Property
prop_list_range_constant =
  property $ do
    x <- forAll $
      Gen.list (Range.singleton 10) SimpleGen.int
    length x === 10

-- |
-- Getting more interesting, we can define generators for enumerations,
-- like characters, numbers, or maybe your own types.
--
prop_enumerations :: Property
prop_enumerations =
  property $ do
    char <- forAll $ Gen.enum 'a' 'f'
    assert $ elem char ['a' .. 'f']


-- |
-- Your own enumeration, for days of the week.
--
data Day =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
    deriving (Bounded, Enum, Eq, Ord, Show)

-- |
-- Generation of enum using your custom type with 'Enum'
--
prop_enumerations_custom :: Property
prop_enumerations_custom =
  property $ do
    enum <- forAll $ Gen.enum Sunday Saturday
    assert $ elem enum [Sunday .. Saturday]

    week <- forAll $ Gen.enum Monday Friday
    assert $ elem week [Monday .. Friday]

-- |
-- Generation of enum using your custom type with 'Enum'
-- and 'Bounded' to get every case.
--
prop_enumerations_custom_bounded :: Property
prop_enumerations_custom_bounded =
  property $ do
    enum <- forAll $ Gen.enumBounded
    assert $ elem enum [Sunday .. Saturday]

-- |
-- We can pick an element from an arbitrary list using
-- 'element'.
--
prop_element :: Property
prop_element =
  property $ do
    x <- forAll $ Gen.element [Tuesday, Thursday, Saturday]
    assert $ elem x [Tuesday, Thursday, Saturday]

-- |
-- We can pick an element from a list of generators using
-- 'choice'.
--
prop_choice :: Property
prop_choice =
  property $ do
    maybeday <- forAll $ Gen.choice [
        Gen.element [Saturday, Sunday]
      , Gen.element [Monday .. Friday]
      ]
    assert $ elem maybeday [Sunday .. Saturday]

-- |
-- We can pick an element from at different frequencies. Here weekends
-- are 10 times more likely to occur than weekdays.
--
prop_frequency :: Property
prop_frequency =
  property $ do
    mostlyweekends <- forAll $ Gen.frequency [
        (10, Gen.element [Saturday, Sunday])
      , (1, Gen.element [Monday .. Friday])
      ]
    assert $ elem mostlyweekends [Sunday .. Saturday]


-- |
-- We can filter results, this is useful to ensure things are
-- unique or some invariant is kept. The below is an example
-- that only generates even numbers.
--
prop_filter :: Property
prop_filter =
  property $ do
    n <- forAll $ Gen.filter isEven $ Gen.int (Range.linear 0 100)
    assert $ isEven n


-- |
-- Filtering can be useful to build generators that depend on each
-- other, for example the below generates two unique numbers, if
-- we didn't filter 'n' and 'm' could end up as the same value.
--
prop_filter_dependence :: Property
prop_filter_dependence =
  property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    m <- forAll $ Gen.filter (/= n) $ Gen.int (Range.linear 0 100)
    assert $ n /= m


-- |
-- Generating strings, is done by specifying the a range for the
-- length of the string and a generator for the characters.
--
prop_string :: Property
prop_string =
  property $ do
    -- Generate a string of only alphabetic characters with a length between 3 and 100.
    s <- forAll $ Gen.string (Range.linear 3 100) Gen.alpha
    assert $ all (\c -> elem c "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") s
    assert $ length s >= 3 && length s <= 100

    -- Generate a string of unicode characters with length of 5.
    unicode <- forAll $ Gen.string (Range.singleton 5) Gen.unicode
    assert $ length unicode == 5

    -- Generate a string of '0', '1' with length of 4.
    binary <- forAll $ Gen.string (Range.singleton 4) (Gen.element "01")
    assert $ all (\c -> elem c "01") binary
    assert $ length binary == 4

    -- Generate an alpha numeric string with new lines and spaces.
    freq <- forAll $ Gen.string (Range.singleton 5) (Gen.frequency [
        (10, Gen.alphaNum)
      , (2, Gen.constant ' ')
      , (1, Gen.constant '\n')
      ])
    assert $ length freq == 5


-- |
-- Simple generator for booleans.
--
prop_bool :: Property
prop_bool =
  property $ do
    b <- forAll Gen.bool
    assert $ b || not b

-- |
-- Generator for maybe.
--
prop_maybe :: Property
prop_maybe =
  property $ do
    n <- forAll $ SimpleGen.int
    m <- forAll $ Gen.maybe (Gen.constant n)
    assert $ m == Nothing || m == Just n

--
-- Often it can be useful to define a function for your generators
-- rather than doing it inline within the property. Here are just some
-- examples so you can see the patterns.
--
-- /note/: You use the same functions, just skip the 'forAll' and the
-- type will be `Gen` of your desired type.
--
genDay :: Gen Day
genDay =
  Gen.enumBounded


--
-- 'Gen' has an Applicative and Monad instance, if you know what that
-- means it will give you some ideas how to compose generators. If
-- not, don't worry about it too much, but understand that you can
-- follow the pattern below to compose the outputs of multiple
-- generators and build generators that depend on each other.
--
genUniqueInts :: Gen (Int, Int)
-- 1. Use 'do' notation to express you 'Gen'
genUniqueInts = do
  -- 2. Every time you want to work with the result
  --    of a generator, use: 'variable <- generator'
  n <- SimpleGen.int
  -- 3. You can refer to variable bindings above your definition.
  m <- Gen.filter (/= n) SimpleGen.int
  -- 4. Use 'pure' to return your result, in this case a pair.
  pure (n, m)


-- == Exercise 4 == --

--
-- We should be able to build some of the simple generators
-- you have seen yourself.
--
-- You can test this with:
--   'Hedgehog.Gen.sample <your-generator>'
-- e.g.
--   'Hedgehog.Gen.sample Test.Generators.genBool'
--

-- |
-- Generate either a 'True' or a 'False'
--
genBool :: Gen Bool
genBool =
  Gen.enumBounded


-- |
-- Generate a 'False' 9 times out of 10, otherwise
-- generate a 'True'
--
genMostlyWrong :: Gen Bool
genMostlyWrong =
  Gen.frequency [
      (9, Gen.constant False)
    , (1, Gen.constant True)
    ]


-- |
-- Generate a 'Nothing' or a 'Just' with a value from the
-- provided generator.
--
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g =
  Gen.choice [
      Gen.constant Nothing
    , fmap Just g
    ]


-- == Exercise 5 == --

-- |
-- We now have everything we need for defining generators for
-- more complicated types.
--
-- @@@
-- data Address =
--   Address {
--       addressNumber :: Int
--     , addressStreet :: String
--     , addressCity :: String
--     , addressCode :: String
--     } deriving (Eq, Ord, Show)
-- @@@
--
genAddress :: Gen Address
genAddress = do
  number <- Gen.int (Range.linear 0 1000)
  street <- Gen.string (Range.linear 3 100) Gen.unicode
  suffix <- Gen.element ["Street", "Road", "Lane", "Highway", "Place", "Drive"]
  city <- Gen.string (Range.linear 3 40) Gen.unicode
  code <- Gen.string (Range.constant 4 5) Gen.digit
  pure $
    Address
      number
      (List.intercalate " " [street, suffix])
      city
      code


-- |
-- To verify your generator. Uncomment the below property.
--
prop_addressIn :: Property
prop_addressIn =
  property $ do

    success
--    x <- forAll genAddress
--    xs <- forAll $ Gen.list (Range.linear 0 10) genAddress
--    let result = livesIn (addressCity x) (x:xs)
--    assert $ length result >= 1
--    assert $ elem x result
--    assert $ all (\a -> addressCity a == addressCity x) result


-- |
-- Try your generator against a buggy version of the code.
--
-- /hint/: Have you tested with unicode?
--
livesIn :: String -> [Address] -> [Address]
livesIn city addresses =
  Generators.livesIn city addresses
--  Generators.livesIn' city addresses


-- |
-- Predicate to determine if an integer is even (used above).
--
isEven :: Int -> Bool
isEven n =
  n `div` 2 == 0

tests :: IO Bool
tests =
  checkParallel $$(discover)
