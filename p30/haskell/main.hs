import Data.Char

d n = map fst $ filter (\(i, s) -> i == s) [(i, sumOfPower n i) | i <- [10..10^(n+1)]]
sumOfPower n = sum . map (^n) . map digitToInt . show