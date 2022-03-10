-- CONSIDER WHICH OBJECTS ARE EXPORTED
{-# LANGUAGE FlexibleInstances #-}
module Serialize where

import Prelude
import Cards
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr)
import Text.Read

cardToBS :: Card -> BS.ByteString
cardToBS card = BS.pack $ show $ card

bsToCard :: BS.ByteString -> Maybe Card
bsToCard bs = readMaybe $ map (chr . fromEnum) . BS.unpack $ bs :: Maybe Card

encodeDeck :: Deck -> [BS.ByteString]
encodeDeck deck = map cardToBS deck
