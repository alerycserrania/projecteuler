import Data.Maybe
import Data.Ord
import Data.List
import System.Environment

main = do
  args <- getArgs
  let decimals = maximumBy (comparing snd)
                . map (\(i, d) -> (i, length (fromJust d))) 
                . filter (isJust . snd) 
                . zip [1..] 
                . map (decimalCycle 1 ) 
                $ [1..(read (head args))]
  print decimals

decimalCycle n d
  | null mDecimalCycle = Nothing
  | otherwise = head mDecimalCycle
  where 
    mDecimalCycle = take 1 . dropWhile isNothing . map decimalCycle' . scanl (flip (:)) [] . tail $ (gen n d)
    decimalCycle' xs 
      | length xs <= 1 = Nothing
      | head xs == last xs = Just (tail xs)
      | otherwise = decimalCycle' (init xs)

gen n d 
  | d > n = (mod n d) : gen (n*10) d
  | mod n d == 0 = [(mod n d)] 
  | otherwise = (mod n d) : gen (10 * mod n d) d

