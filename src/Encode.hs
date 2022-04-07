{-# LANGUAGE FlexibleInstances #-}
module Encode where

import Prelude
import Cards
import Data.ByteString.Conversion (toByteString, fromByteString)

b :: Card
b = (Club, Seven)
a = toByteString $ show b

class Serializable a where
    encode :: a -> Integer
    decode :: Integer -> a

-- | The suits are encode encode prime numbers. To create an bijection between the 52 cards and the integers (for ElGamal encryption) the product of the serialization is used.
instance Serializable Suit where
    encode Club         = 29 
    encode Diamond      = 31
    encode Heart        = 37
    encode Spade        = 41
    decode 29         = Club
    decode 31         = Diamond
    decode 37         = Heart
    decode 41         = Spade

instance Serializable Value where
    encode Two   = 2
    encode Three = 3
    encode Four  = 4
    encode Five  = 5
    encode Six   = 6
    encode Seven = 7
    encode Eight = 8
    encode Nine  = 9
    encode Ten   = 10
    encode Jack  = 11
    encode Queen = 12
    encode King  = 13
    encode Ace   = 14
    decode 2   = Two
    decode 3   = Three
    decode 4   = Four
    decode 5   = Five
    decode 6   = Six
    decode 7   = Seven
    decode 8   = Eight
    decode 9   = Nine
    decode 10  = Ten
    decode 11  = Jack
    decode 12  = Queen
    decode 13  = King
    decode 14  = Ace

instance Serializable Card where
    encode (suit, value) = (encode suit) * (encode value)
    decode n
        |       primeFactor 29         = (Club, decode $ factorDel 29)
        |       primeFactor 31         = (Diamond, decode $ factorDel 31)
        |       primeFactor 37         = (Heart, decode $ factorDel 37)
        |       primeFactor 41         = (Spade, decode $ factorDel 41)
                where
                        primeFactor x = n `mod` x == 0
                        factorDel y = n `div` y

encodeDeck :: Deck -> [Integer]
encodeDeck deck = map encode deck

decodeDeck :: [Integer] -> Deck
decodeDeck encodedDeck = map decode encodedDeck

