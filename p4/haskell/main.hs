import System.Environment

main = do
  args <- getArgs
  let ns = gen (read (head args) :: Int)
  print (maximum . filter palindrome . map (uncurry (*)) $ cartProd ns ns)

palindrome n = palindromeStr . show $ n

palindromeStr n = n == reverse n

gen n = [10^(n-1)..10^(n)-1]

cartProd xs ys = [(x, y) | x <- xs, y <- ys, x <= y]
