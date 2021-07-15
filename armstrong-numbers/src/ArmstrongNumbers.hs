module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ length digits) (digits))
              where
                base = 10
                listDigits x ds | x < base = x : ds
                                | otherwise = listDigits (x `div` base) ((x `mod` base) : ds)
                digits = listDigits (abs n) []

