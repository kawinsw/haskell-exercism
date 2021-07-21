module Phone (number) where

number :: String -> Maybe String
number xs = if null nineDigits || head nineDigits < '2' || nineDigits !! 3 < '2'
            then Nothing else Just nineDigits
            where
              nums = filter (\c -> ('0' <= c) && (c <= '9')) xs
              len = length nums
              nineDigits | len == 10 = nums
                         | len == 11 && head nums == '1' = tail nums
                         | otherwise = ""
