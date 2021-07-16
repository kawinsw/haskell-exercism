module Raindrops (convert) where

convert :: Int -> String
convert n = if null rainString'
            then show n
            else rainString'
            where
              rainString' = rainString n

rainString :: Int -> String
rainString n = rainPart 3 "Pling" ++ rainPart 5 "Plang" ++ rainPart 7 "Plong"
               where
                 rainPart = strIfMultiple n

strIfMultiple :: Int -> Int -> String -> String
strIfMultiple n k xs = if n `mod` k == 0 then xs else ""
