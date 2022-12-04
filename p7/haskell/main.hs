import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Integer
  print (nthPrime n)

nthPrime n = nthPrime' n 1

nthPrime' 0 p = p 
nthPrime' n p = nthPrime' (n-1) (nextPrime (p+1))

nextPrime p 
  | isPrime p = p
  | otherwise = nextPrime (p+1)

isPrime 1 = False
isPrime p = length (factors p) == 0 

factors p = factors' p 2
factors' p d
  | (sqrtInt p) < d = []
  | mod p d == 0 = d : (div p d) : factors' p (d+1)
  | otherwise = factors' p (d+1)

sqrtInt = floor . sqrt . fromInteger 