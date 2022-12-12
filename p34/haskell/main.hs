
solutions = [i | i <- [10..2177280], (sum . map (factorial) . decompose $ i) == i]

decompose n 
  | n > 9 = (mod n 10) : decompose (div n 10)
  | otherwise = [n]
-- nbDigits n 
--   | n > 9 = 1 + nbDigits (div n 10)
--   | otherwise = 1

factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial 6 = 720
factorial 7 = 5040
factorial 8 = 40320
factorial 9 = 362880

limit = limit' 1
  where 
    limit' n 
      | n * (factorial 9) < (10^n) = n
      | otherwise = limit' (n+1)
