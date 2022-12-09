import Data.Char
import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let names = parse (head input)
  print (sum . map (uncurry (*)) . enumerate . map nameScore . sort $ names)


nameScore = sum . map charScore 
charScore = ((+)(-64)) . ord

enumerate ls = zip [1..] ls

parse s = map (init . tail) . split ',' $ s

split c s
  | null s = []
  | isJust mPosC = (take posC s) : split c (drop (posC+1) s)
  | otherwise = [s]
  where mPosC = elemIndex c s
        posC = fromJust mPosC

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)