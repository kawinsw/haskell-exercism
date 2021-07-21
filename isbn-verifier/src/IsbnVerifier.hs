module IsbnVerifier (isbn) where

import Data.Char (digitToInt)

isbn :: String -> Bool
isbn xs = length code == 10 && all (\n -> n < 10) (init code) && total `mod` 11 == 0
          where
            code = map toInt (filter isValidChar xs)
            total = sum (map (\tup -> fst tup * snd tup) (zip code [10, 9..1]))

isValidChar :: Char -> Bool
isValidChar c = ('0' <= c) && (c <= '9') || c == 'X'

toInt :: Char -> Int
toInt 'X' = 10
toInt c = digitToInt c
