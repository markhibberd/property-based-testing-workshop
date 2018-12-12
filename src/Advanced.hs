--
-- ==========================================
-- Example functions only:
--    Workshop Exercises are in 'test/Test'
-- ==========================================
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Advanced (
    Product (..)
  , Dollars (..)
  , Item (..)
  , Order (..)
  , Register (..)
  , Cash (..)

  , total
  , total'
  , total''

  , aggregate

  , discount
  , discount'
  , discount''
  , discount'''
  , discount''''

  , change
  , change'

  , merge
  , merge'
  ) where


import           Control.Monad (join)
import           Data.List (foldl, nubBy, sortOn, subsequences, groupBy, group)
import           Data.Function (on)

newtype Product =
    Product String
    deriving (Eq, Ord, Show)

newtype Dollars =
    Dollars Int
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data Item =
    Item {
        itemProduct :: Product
      , itemDollars :: Dollars
      } deriving (Eq, Ord, Show)

newtype Order =
    Order [(Int, Item)]
    deriving (Eq, Ord, Show)

newtype Register =
    Register Cash
    deriving (Eq, Ord, Show)

newtype Cash =
    Cash [(Int, Dollars)]
    deriving (Eq, Ord, Show)

total :: Order -> Dollars
total (Order items) =
  sum $ fmap (\(qty, item) -> fromIntegral qty * itemDollars item) items

total' :: Order -> Dollars
total' (Order items) =
  sum $ fmap (\(_qty, item) -> itemDollars item) items

total'' :: Order -> Dollars
total'' (Order items) =
  sum $ fmap (\(qty, item) -> (fromIntegral $ qty `mod` 5) * itemDollars item) items

aggregate :: Order -> Order
aggregate (Order items) =
  Order . foldl (\acc el -> case el of
    [] ->
      acc
    (x:xs) ->
      (sum (fmap fst (x:xs)), snd x) : acc) [] . groupBy (\(_, Item a _) (_, Item b _) -> a == b) . sortOn (itemProduct . snd) $ items

discount :: Order -> Order
discount order =
  case aggregate order of
    Order items ->
      Order $ map (\(qty, Item p price) ->
        if qty > 5 && price >= 10 then
          (qty, Item p (price - 5))
        else
          (qty, Item p price)) items

discount' :: Order -> Order
discount' order =
  case aggregate order of
    Order items ->
      Order $ map (\(qty, Item p price) ->
        (qty, Item p (price - 5))) items

discount'' :: Order -> Order
discount'' order =
  case aggregate order of
    Order items ->
      Order $ map (\(qty, Item p price) ->
        if qty > 5 && price > 10 then
          (qty, Item p (price - 5))
        else
          (qty, Item p price)) items

discount''' :: Order -> Order
discount''' order =
  case aggregate order of
    Order items ->
      Order $ map (\(qty, Item p price) ->
        if qty >= 5 && price >= 10 then
          (qty, Item p (price - 5))
        else
          (qty, Item p price)) items

discount'''' :: Order -> Order
discount'''' order =
  case aggregate order of
    Order items ->
      Order $ map (\(index, (qty, Item p price)) ->
        if qty > 5 && price >= 10 && index < 10 then
          (qty, Item p (price - 5))
        else
          (qty, Item p price)) (zip [0..] items)

change :: Register -> Cash -> Dollars -> Maybe Cash
change (Register (Cash register)) (Cash cash) total =
  let
    -- construct a flat list of all of the available notes.
    notes = join (fmap (\(qty, dollars) -> replicate qty dollars) (register ++ cash))
    -- work out how much change they need
    remainder = sum (fmap (\(qty, dollars) -> fromIntegral qty * dollars) cash) - total
    -- what are all the possible change combinations
    options = subsequences notes
    -- filter to those for the right amount
    valid = filter (\ds -> sum ds == remainder) options
    -- sort by the smallest number of notes change
    ordered = sortOn length valid
  in
    case ordered of
      [] ->
        Nothing
      (x:_) ->
        Just (Cash (join (fmap (\xs -> case xs of
                                [] -> []
                                (z:zs) -> [(length (z:zs), z)]) (group x))))

change' :: Register -> Cash -> Dollars -> Maybe Cash
change' (Register (Cash register)) (Cash cash) total =
  let
    -- construct a flat list of all of the available notes.
    notes = join (fmap (\(qty, dollars) -> replicate qty dollars) register)
    -- work out how much change they need
    remainder = sum (fmap (\(qty, dollars) -> fromIntegral qty * dollars) cash) - total
    -- what are all the possible change combinations
    options = subsequences notes
    -- filter to those for the right amount
    valid = filter (\ds -> sum ds == remainder) options
    -- sort by the smallest number of notes change
    ordered = sortOn length valid
  in
    case ordered of
      [] ->
        Nothing
      (x:_) ->
        Just (Cash (join (fmap (\xs -> case xs of
                                [] -> []
                                (z:zs) -> [(length (z:zs), z)]) (group x))))

merge :: Order -> Order -> Order
merge (Order a) (Order b) =
  aggregate (Order (a ++ b))

merge' :: Order -> Order -> Order
merge' (Order a) (Order b) =
  Order (nubBy ((==) `on` (itemProduct . snd)) (a ++ b))
