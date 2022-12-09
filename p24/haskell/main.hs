main = do
  print ((permutations [0..9])!!999_999)


permutations [] = []
permutations [x] = [[x]]
permutations ls = [x:pxs | i <- map snd . zip ls $ [0..], let (x, xs) = (popAt i ls), pxs <- (permutations xs)]

popAt i xs = (xs!!i, f ++ (tail s)) 
  where (f, s) = splitAt i xs