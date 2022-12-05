import System.IO

main = do
  input <- getInput
  let result = take 10 . show . sum . map (read :: String -> Integer) $ input
  print (result)

parseInput s =  s :: Integer

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)