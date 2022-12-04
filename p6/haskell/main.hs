import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Int
  print ((squareOfSum n) - (sumOfSquares n))

sumOfSquares n = sum (map (\x -> x^2) [1..n])
squareOfSum n = sum [1..n] ^ 2