module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter | toUpper(letter) `elem` "AEIOULNRST" = 1
                   | toUpper(letter) `elem` "DG"         = 2
                   | toUpper(letter) `elem` "BCMP"       = 3
                   | toUpper(letter) `elem` "FHVWY"      = 4
                   | toUpper(letter) == 'K'              = 5
                   | toUpper(letter) `elem` "JX"         = 8
                   | toUpper(letter) `elem` "QZ"         = 10
                   | otherwise                           = 0

scoreWord :: String -> Integer
scoreWord word = sum (map scoreLetter word)
