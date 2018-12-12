--
-- ==========================================
-- Example functions only:
--    Workshop Exercises are in 'test/Test'
-- ==========================================
--
module Patterns (
    slidingAll
  ) where

-- |
-- Determines if all elements in a list satisfy a
-- predicate. 'slidingAll' works like the standard library 'all'
-- function except that it also provides you access to the 'previous'
-- value in the list (if there was one).
--
-- /note/: Helper for verification pattern.
--
slidingAll :: (Maybe a -> a -> Bool) -> [a] -> Bool
slidingAll p list =
  snd $ foldl (\(previous, acc) el -> (Just el, acc && p previous el)) (Nothing, True) list
