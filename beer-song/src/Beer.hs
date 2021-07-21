module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n" (map songLine [99, 98 .. 0])

songLine :: Int -> String
songLine n = numLeft n True ++ beerWall ++ comma ++ 
             numLeft n False ++ beer ++ lineEnd ++
             action n ++ comma ++ 
             numLeft (n-1) False ++ beerWall ++ lineEnd

beerWall :: String
beerWall = beer ++ " on the wall"

beer :: String
beer = " of beer"

comma :: String
comma = ", "

lineEnd :: String
lineEnd = ".\n"

qty :: Int -> Bool -> String
qty (-1) _ = "99"
qty 0 True = "No more"
qty 0 False = "no more"
qty n _ = show n

bottle :: Int -> String
bottle 1 = " bottle"
bottle _ = " bottles"

numLeft :: Int -> Bool -> String
numLeft n caps = qty n caps ++ bottle n

one :: Int -> String
one 1 = "it"
one _ = "one"

action :: Int -> String
action 0 = "Go to the store and buy some more"
action n = "Take " ++ one n ++ " down and pass it around"
