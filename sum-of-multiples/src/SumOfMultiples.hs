module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [1..(limit - 1)], any (x `isMultipleOf`) factors]
                               where
                                 isMultipleOf n d | d == 0 = False
                                                  | otherwise = n `mod` d == 0
