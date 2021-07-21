module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show


label :: Resistor -> String
label resistor | n > 10^9 = show (n `div` 10^9) ++ " gigaohms"
               | n > 10^6 = show (n `div` 10^6) ++ " megaohms"
               | n > 10^3 = show (n `div` 10^3) ++ " kiloohms"
               | otherwise = show n ++ " ohms"
               where n = ohms resistor

ohms :: Resistor -> Int
ohms resistor = (10 * tens + ones) * 10 ^ expo
                 where
                   (tens:ones:expo:_) = resistorValues resistor colorValue

resistorValues :: Resistor -> (Color -> a) -> [a]
resistorValues r f = map f [tens, ones, expo]
                     where
                       (tens, ones, expo) = bands r

colorValue :: Color -> Int
colorValue Black = 0
colorValue Brown = 1
colorValue Red = 2
colorValue Orange = 3
colorValue Yellow = 4
colorValue Green = 5
colorValue Blue = 6
colorValue Violet = 7
colorValue Grey = 8
colorValue White = 9
