import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Integer
  print (path n [])


-- efficient recursion
path n xs
  | n == 0 =  last xs
  | otherwise = path (n-1) (path' xs)
  where 
    path' xs = (tail ns) ++ [2 * last ns]
      where 
        ns = foldl (\rdc new -> rdc ++ [new + last rdc]) [1] xs

-- inefficient 
-- path n xs ys = (head p3 + head p4, p3, p4)
--   where p1 = map (+1) (xs)
--         p2 = map (+1) (ys)
--         p3 = (n + head p1) : p1
--         p4 = (n + head p2) : p2