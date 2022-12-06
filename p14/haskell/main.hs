import Data.Tuple
import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Int
  print (maximum . map swap . indexed $ (map collatzchains [1..n]))


indexed l = indexed' l 1
  where 
    indexed' l' i
      | null l' = []
      | otherwise = (i, head l') : indexed' (tail l') (i+1)

collatzchains n = 1 + (length . takeWhile (>1) . scanl (\a _ -> collatz a) (n) $ [1..])

collatz n
  | mod n 2 == 0 = div n 2
  | otherwise = 3 * n + 1