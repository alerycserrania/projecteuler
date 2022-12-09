import qualified Data.Set as S

main = do
  let (mini, maxi) = (12, 28123)
  let abundants = (filter abundant [mini..maxi])
  print (sum . inter 0 28123 . S.toList . S.fromList . filter ((>=)maxi) . crossSum $ abundants)
  

inter min max (a:xs) = [0..(a-1)] ++ (inter' ((a+1):xs))
  where 
    inter' (x:[]) = [x..max]
    inter' (x:y:ys) = [x..(y-1)] ++ (inter' ((y+1):ys))

crossSum a 
  | null a = []
  | otherwise = map ((+) (head a)) a ++ crossSum (tail a)
  
perfect n = d(n) == n
abundant n = d(n) > n
deficient n = d(n) < n 

d n = sum . filter (/=n) . d' $ 1 
  where
    d' i
      | i * i > n = []
      | mod n i == 0 = d' (i+1) ++ 
        if (div n i) == i 
          then [i] 
          else [(div n i), i] 
      | otherwise = d' (i+1)