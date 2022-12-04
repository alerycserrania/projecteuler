main = do
  let (a, b, c) = head (genTriplets 1000)
  print (a * b * c)

genTriplets n = [(a,b,(n-a-b)) | a <- [0..n], b <- [a..n], (n-a-b) > b, a^2 + b^2 == (n-a-b)^2]
  