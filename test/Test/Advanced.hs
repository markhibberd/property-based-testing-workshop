{-# LANGUAGE TemplateHaskell #-}
module Test.Advanced where

import           Advanced (Register (..), Cash (..), Dollars (..), Product (..), Item (..), Order (..))
import qualified Advanced as Advanced

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- == Exercise 18 ==

--
-- This is an example that moves past lists, and is closer to
-- functions you might be faced with in your day-to-day work.
--
-- The example is centered around a point-of-sale system, that
-- involves orders and payments. The example includes both
-- working and buggy versions of the code.
--
-- The functions are stubbed out in this module to make it
-- easy to see the type signatures and switch between the
-- working and buggy versions of the code.
--
-- This is exercise is in three parts:
--  1. Construct generators for your types.
--  2. Test all of the functions.
--  3. Verify your tests against buggy implementations.
--
-- Can you construct generators and properties that will
-- let you find the bug without even looking at the code?
--

-- |
-- Determine the total cost of an order.
--
total :: Order -> Dollars
total order =
  Advanced.total order
--  Advanced.total' order
--  Advanced.total'' order


-- |
-- Apply a $5 discount to items when at least 5
-- of that item has been ordered.
--
-- We should not discount items that cost less than
-- $10 per item.
--
discount :: Order -> Order
discount order =
  Advanced.discount order
--  Advanced.discount' order
--  Advanced.discount'' order
--  Advanced.discount''' order
--  Advanced.discount'''' order


-- |
-- Merge two orders into one order.
--
merge :: Order -> Order -> Order
merge a b =
  Advanced.merge a b
--  Advanced.merge' order

-- |
-- Determine the change for a customer (if possible).
--
-- Given the current cash in the register, take
-- the payment cash from the customer, and the
-- dollar amount they have to pay, and determine
-- the cash they get as change.
--
-- Cash is indivisible (we can't rip a note in two),
-- so you can only return full notes you have available.
--
-- Example:
-- @@@
--  change (Register (Cash [(2, Dollars 20), (1, Dollars 10)]))
--         (Cash [(1, Dollars 50)])
--         (Dollars 20) == Cash [(1, Dollars 20), (1, Dollars 10)]
-- @@@
--
change :: Register -> Cash -> Dollars -> Maybe Cash
change register payment cost =
  Advanced.change register payment cost
--  Advanced.change' register payment cost


-- First we have to construct generators for our types.

-- |
-- A product name.
--
-- > newtype Product = Product String
--
genProduct :: Gen Product
genProduct =
  -- WORKSHOP EXERCISE
  error "todo"

-- |
-- A positive whole dollar amount.
--
-- > newtype Dollars = Dollars Int
--
genDollars :: Gen Dollars
genDollars =
  -- WORKSHOP EXERCISE
  error "todo"


-- |
-- An item is a product with a price.
--
-- > data Item = Item Product Dollars
--
genItem :: Gen Item
genItem =
  -- WORKSHOP EXERCISE
  error "todo"

-- |
-- An order is a collection of items with their quantities.
--
-- > newtype Order = Order [(Int, Item)]
--
genOrder :: Gen Order
genOrder =
  -- WORKSHOP EXERCISE
  error "todo"

-- |
-- Cash is a collection of note denominations with quantities.
--
-- Valid dollar amounts are '10', '20', '50' and '100'.
--
-- > newtype Cash = Cash [(Int, Dollars)]
--
genCash :: Gen Cash
genCash =
  -- WORKSHOP EXERCISE
  error "todo"

-- |
-- A register just contains some 'Cash'.
--
-- > newtype Register = Register Cash
--
genRegister :: Gen Register
genRegister =
  fmap Register genCash



--
-- Properties for 'merge'.
--

-- WORKSHOP EXERCISE


-- Without looking at the code:
--   What bug do you think merge' has?

--
-- Properties for 'total'.
--

-- WORKSHOP EXERCISE


-- Without looking at the code:
--   What bug do you think total' has?
--   What bug do you think total'' has?


--
-- Properties for 'discount'.
--

-- WORKSHOP EXERCISE


-- Without looking at the code:
--   What bug do you think discount' has?
--   What bug do you think discount'' has?
--   What bug do you think discount''' has?
--   What bug do you think discount'''' has?

--
-- Properties for 'change'.
--
-- /note/: This is a particularly challenging problem.
--

-- WORKSHOP EXERCISE


-- Without looking at the code:
--   What bug do you think change' has?
--   What bug do you think change'' has?

tests :: IO Bool
tests =
  checkParallel $$(discover)
