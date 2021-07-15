module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = square ((n * (n + 1)) `div` 2)

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (n * (n + 1) * (2*n + 1)) `div` 6

square :: Integral a => a -> a
square n = n * n
