{-# LANGUAGE OverloadedStrings #-}

module Cards where

import Prelude
import System.Random
import Data.Map hiding (foldl)
import Data.Serialize
import Crypto.Random

data Suit = Club | Diamond | Heart | Spade deriving (Enum, Read)

data Value = Two | Three | Four | Five | Six | Seven
              | Eight | Nine | Ten | Jack | Queen
              | King | Ace deriving (Enum, Read)

type Card = (Suit, Value)
type Deck = [Card]

instance Show Suit where
   show Club = "Club"
   show Diamond = "Diamond"
   show Heart = "Heart"
   show Spade = "Spade"

instance Show Value where
  show Two = "Two"
  show Three = "Three"
  show Four = "Four"
  show Five = "Five"
  show Six = "Six"
  show Seven = "Seven"
  show Eight = "Eight"
  show Nine = "Nine"
  show Ten = "Ten"
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"
  show Ace = "Ace"

makeDeck :: Deck
makeDeck = [(suit, value) | suit <- [Club .. Spade], value <- [Two .. Ace]]

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)

shuffle :: [a] -> IO [a] 
shuffle list = do
  g <- getStdGen
  newStdGen
  return $ fst $ fisherYates g list
