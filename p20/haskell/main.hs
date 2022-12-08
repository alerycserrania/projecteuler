import Data.Char

main = do
  print (sum . map digitToInt . show . fact $ 100)


fact 0 = 1
fact n = n * fact (n-1)