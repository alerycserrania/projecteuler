import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let lines = reverse . map (split ' ') $ input
  print (foldl doit (head lines) (tail lines))

doit l1 l2 = sums (maxs l1) l2
maxs line = map (uncurry max) (zip (init line) (tail line)) 

sums [] [] = []
sums (x:xs) (y:ys) = (x+y) : sums xs ys

split c l
  | isNothing i = [read l :: Int]
  | otherwise =  (read f :: Int) : split c (tail s)
    where i = elemIndex c l
          (f, s) = splitAt (fromJust i) l

getInput = do
  done <- isEOF
  if done 
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)