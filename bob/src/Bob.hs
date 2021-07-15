module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs = responseFor' ([x | x <- xs, not (isSpace x)])

responseFor' :: String -> String
responseFor' xs | null xs = "Fine. Be that way!"
                | isYell xs = if last xs == '?' 
                              then "Calm down, I know what I'm doing!"
                              else "Whoa, chill out!"
                | last xs == '?' = "Sure."
                | otherwise = "Whatever."

isYell :: String -> Bool
isYell xs = not (any isLower xs) && any isUpper (tail xs)
