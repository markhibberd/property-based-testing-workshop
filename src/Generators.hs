--
-- ==========================================
-- Example functions only:
--    Workshop Exercises are in 'test/Test'
-- ==========================================
--
module Generators (
    Address (..)
  , livesIn
  , livesIn'
  ) where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


data Address =
  Address {
      addressNumber :: Int
    , addressStreet :: String
    , addressCity :: String
    , addressCode :: String
    } deriving (Eq, Ord, Show)

livesIn :: String -> [Address] -> [Address]
livesIn city addresses =
  filter (\a -> addressCity a == city) addresses

livesIn' :: String -> [Address] -> [Address]
livesIn' city addresses =
  filter (\a -> addressCity a == asciify city) addresses

asciify :: String -> String
asciify t =
  Text.unpack (Text.decodeLatin1 (Text.encodeUtf8 (Text.pack t)))
