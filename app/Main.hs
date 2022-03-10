-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import Cards 
import Encode
import Cryptography
import Crypto.PubKey.ElGamal as ElGamal
import Crypto.Random.Types

main :: IO ()
main = do
        deckS <- shuffle makeDeck
        int <- getLine
        let integer = read int :: Int
        putStr $ show $ makeDeck !! integer
        putStr "\n"
        print $ show $ encode $ makeDeck !! integer
        ElGamal.encrypt unsafeParams unsafePubKey1 (encode $ makeDeck !! integer) >>= print

