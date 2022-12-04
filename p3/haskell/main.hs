import System.Environment

main = do
  args <- getArgs
  print (last . factors $ (read (head args) :: Int))

factors n = factors' n 2 

factors' n f
  | n <= 1  = []
  | (mod n f == 0) = f : factors' (div n f) f
  | otherwise = factors' n (f+1)
