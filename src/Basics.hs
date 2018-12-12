--
-- ==========================================
-- Example functions only:
--    Workshop Exercises are in 'test/Test'
-- ==========================================
--
module Basics (
    size'
  , size''
  , headOr'
  , headOr''
  , total'
  , total''
  , total'''
  ) where


size' :: [Int] -> Int
size' list =
  case list of
    [] ->
      0
    (_:xs) ->
      1 + size' xs

size'' :: [Int] -> Int
size'' list =
  case list of
    [] ->
      0
    (_:xs) ->
      1 + (size'' xs * size'' xs)

headOr' :: a -> [a] -> a
headOr' dfault list =
  case list of
    [] ->
      dfault
    (x:_xs) ->
      x

headOr'' :: a -> [a] -> a
headOr'' dfault list =
  case list of
    [] ->
      dfault
    (_x:y:_xs) ->
      y
    (x:_xs) ->
      x

total' :: [Int] -> Int
total' list =
  case list of
    [] ->
      0
    (x:xs) ->
      x + total' xs

total'' :: [Int] -> Int
total'' list =
  case list of
    [] ->
      0
    (x:xs) ->
      x + abs (total'' xs)

total''' :: [Int] -> Int
total''' list =
  case list of
    [] ->
      0
    (x:xs) ->
      if length xs > 10 then
        x + abs (total''' xs)
      else
        x + total''' xs
