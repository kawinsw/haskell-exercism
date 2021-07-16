module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList (foldr (++) [] (map expandSwap (toList legacyData)))

expandSwap :: (a, String) -> [(Char, a)]
expandSwap (x, ys) = map (\z -> (toLower z, x)) ys
