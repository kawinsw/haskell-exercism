module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c | any (\x -> x < 0) arr || all (\x -> x == 0) arr = Illegal
                   | lo == hi                                        = Equilateral
                   | lomid < hi                                      = Illegal
                   | lo == mid || mid == hi                          = Isosceles
                   | otherwise                                       = Scalene
                   where
                     arr = [a, b, c]
                     total = a + b + c
                     lo = min a (min b c)
                     hi = max a (max b c)
                     lomid = total - hi
                     mid = lomid - lo
