import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Int
  print (head (dropWhile (hasOverNDiv n) (genTriangle)))

genTriangle = scanl (\x i -> x + i) (0) [1..]

hasOverNDiv n d = length (divisors d) < n

divisors n = divisors' n 1
  where 
    divisors' n d
      | d*d > n = []
      | d*d == n = d : rest
      | mod n d == 0 = d : (div n d) : rest
      | otherwise = rest
        where rest = divisors' n (d+1)
