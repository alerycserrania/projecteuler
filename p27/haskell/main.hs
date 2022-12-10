import Data.List
import qualified Data.Set as S
import Data.Ord

main = do
  print (maximumBy (comparing snd) 
        . map (\(a,b) -> ((a,b), (nbPrimes a b))) 
        $ [(a, b) | a <- [-999..999], b <- [-999..999]])

nbPrimes a b = length . takeWhile (isPrime) . map (quadratic a b)  $ [0..]

quadratic a b n = (n^2) + a*n + b

-- isPrime' n = if n >= 2 then primes!!n else False
isPrime n 
  | n >= 2 = null . filter ((==0) . (n `mod`)) . takeWhile ((<=n) . (^2)) $ [2..]
  | otherwise = False