import Data.Char
import System.IO
import System.Environment

main = do
  args <- getArgs
  input <- getInput
  let n = read (head args) :: Int
  print (maximum . map prodOfDigits . groups n . concat $ input)

groups n xs
  | n >= length xs = [xs]
  | otherwise = (take n xs) : (groups n (tail xs))

prodOfDigits [] = 1
prodOfDigits (d:ds) = (digitToInt d) * (prodOfDigits ds)

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)