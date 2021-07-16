module DNA (toRNA) where

bases :: String
bases = "ATCG"

validPlaceholder :: Char
validPlaceholder = 'A'

transcribe 'A' = 'U'
transcribe 'T' = 'A'
transcribe 'C' = 'G'
transcribe 'G' = 'C'

toRNA :: String -> Either Char String
toRNA xs = if firstInvalid == validPlaceholder
           then Right (map transcribe xs)
           else Left firstInvalid
           where
             firstInvalid :: Char
             firstInvalid = invalidDNA xs

invalidDNA :: String -> Char
invalidDNA xs = if null xs
                then 'A'
                else if elem x bases
                then invalidDNA (tail xs)
                else x
                where
                  x :: Char
                  x = head xs
