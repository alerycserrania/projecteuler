import System.Environment

main = do
  args <- getArgs
  let n = read (head args)
  print (sum (diag n))

diag n = 1 : take (4*(div n 2)) (gen 1 2)


gen s n = ls ++ gen (last ls) (n+2) 
    where ls = [s+i*n | i <- [1..4]]