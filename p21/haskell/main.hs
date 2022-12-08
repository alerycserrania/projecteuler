
main = do
  print (sum (filter (amicable) ([1..10000]) ))

amicable a = a /= b && d(a) == b && d(b) == a
  where b = d(a)

d n = sum . filter (/=n) . d' $ 1 
  where
    d' i
      | i * i > n = []
      | mod n i == 0 = i : (div n i) : d' (i+1)
      | otherwise = d' (i+1)