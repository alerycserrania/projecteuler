
import Data.List
import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Int
  -- print (sum . filter isPrime $ [2..n])
  print (sum (primes n))

primes n = fill (sort (sieve 2 n [])) 2 n
  where
    sieve p n xs 
      | p*p >= n = xs
      | not (elem p xs) = sieve (p+1) n (merge [p*p,p*p+p..n] xs)
      | otherwise = sieve (p+1) n xs 

fill xs n m
  | null xs = [n..m]
  | (head xs) == n = fill (tail xs) (n+1) m
  | otherwise = n : fill xs (n+1) m

merge o1 [] = o1
merge [] o2 = o2
merge o1 o2 = merge' o1 o2
  where 
    merge' (x:xs) (y:ys)
      | x < y = x : merge xs (y:ys)
      | x == y = merge (x:xs) ys
      | otherwise = y : merge (x:xs) ys

isPrime p = isPrime' 2
  where 
    isPrime' d
      | d*d > p = True
      | mod p d == 0 = False
      | otherwise = isPrime' (d+1)

