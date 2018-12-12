--
-- ==========================================
-- Example functions only:
--    Workshop Exercises are in 'test/Test'
-- ==========================================
--
module Practice (
    flatten
  , factorial
  ) where


factorial :: Int -> Int
factorial n =
  if n == 0 then
    1
  else
    n * factorial (n - 1)

flatten :: [[a]] -> [a]
flatten lists =
  case lists of
    [] ->
      []
    (x:xs) ->
      x ++ flatten xs
