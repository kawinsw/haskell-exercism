module Yacht (yacht, Category(..)) where

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht Ones dice             = countFace dice 1
yacht Twos dice             = 2 * countFace dice 2
yacht Threes dice           = 3 * countFace dice 3
yacht Fours dice            = 4 * countFace dice 4
yacht Fives dice            = 5 * countFace dice 5
yacht Sixes dice            = 6 * countFace dice 6
yacht FullHouse dice        = if 2 `elem` tally && 3 `elem` tally then sum dice else 0
                              where tally = map (countFace dice) [1..6]
yacht FourOfAKind dice      = if null fourFaces then 0 else 4 * head fourFaces
                              where fourFaces = filter (\n -> countFace dice n >= 4) [1..6]
yacht LittleStraight dice   = if all (\n -> n `elem` dice) [1..5] then 30 else 0
yacht BigStraight dice      = if all (\n -> n `elem` dice) [2..6] then 30 else 0
yacht Choice dice           = sum dice
yacht Yacht dice            = if any (\face -> all (\n -> n == face) dice) [1..6] then 50 else 0

countFace :: [Int] -> Int -> Int
countFace dice y = length (filter (\x -> x == y) dice)
