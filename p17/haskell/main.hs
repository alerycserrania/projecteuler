import Data.List
import System.Environment

main = do
  args <- getArgs
  let n = read (head args) :: Int
  print (sum . map length . map concat . map toWords $ [1..n])

unit 1 = "one"
unit 2 = "two"
unit 3 = "three"
unit 4 = "four"
unit 5 = "five"
unit 6 = "six"
unit 7 = "seven"
unit 8 = "eight"
unit 9 = "nine"
unit 10 = "ten"
unit 11 = "eleven"
unit 12 = "twelve"
unit 13 = "thirteen"
unit 14 = "fourteen"
unit 15 = "fifteen"
unit 16 = "sixteen"
unit 17 = "seventeen"
unit 18 = "eighteen"
unit 19 = "nineteen"

ten 2 = "twenty"
ten 3 = "thirty"
ten 4 = "forty"
ten 5 = "fifty"
ten 6 = "sixty"
ten 7 = "seventy"
ten 8 = "eighty"
ten 9 = "ninety"

hundred = "hundred"
thousand = "thousand"

toWords n 
  | n == 0 = []
  | n < 20 = [unit n]
  | n < 100 = ten (div n 10) : toWords (mod n 10)
  | n < 1000 = unit (div n 100) : hundred : (if tens /= 0 then ("and" : toWords tens) else [])
  | n == 1000 = [unit 1, thousand]
    where tens = mod n 100

