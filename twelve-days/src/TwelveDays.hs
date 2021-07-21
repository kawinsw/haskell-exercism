module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map songLine [start..stop]

songLine :: Int -> String
songLine n = "On the " ++ ordinalNum n ++ 
             " day of Christmas my true love gave to me: " ++
             gifts ++ "a Partridge in a Pear Tree."
             where 
               gifts = intercalate ", " (map linePart [n, (n-1)..2])

linePart :: Int -> String
linePart 2 = "two Turtle Doves, and "
linePart 3 = "three French Hens"
linePart 4 = "four Calling Birds"
linePart 5 = "five Gold Rings"
linePart 6 = "six Geese-a-Laying"
linePart 7 = "seven Swans-a-Swimming"
linePart 8 = "eight Maids-a-Milking"
linePart 9 = "nine Ladies Dancing"
linePart 10 = "ten Lords-a-Leaping"
linePart 11 = "eleven Pipers Piping"
linePart 12 = "twelve Drummers Drumming"

ordinalNum :: Int -> String
ordinalNum 1 = "first"
ordinalNum 2 = "second"
ordinalNum 3 = "third"
ordinalNum 4 = "fourth"
ordinalNum 5 = "fifth"
ordinalNum 6 = "sixth"
ordinalNum 7 = "seventh"
ordinalNum 8 = "eighth"
ordinalNum 9 = "ninth"
ordinalNum 10 = "tenth"
ordinalNum 11 = "eleventh"
ordinalNum 12 = "twelfth"
