import System.IO

main = do
  input <- getInput
  let grid = map (\x -> map read (words x) :: [Int]) input
  print (maximum (allProducts grid 4))
  
allProducts grid n = [product (dir grid n row col) | dir <- [down, rightDown, right, rightUp],
                                                     row <- [0..(length grid)-1], 
                                                     col <- [0..(length (grid!!row)-1)] ] 

down grid n row col = [grid!!(row+i)!!col | i <- [0..(n-1)], (row+i) < length grid, col < length (grid!!(row+i))]
rightDown grid n row col = [grid!!(row+i)!!(col+i) | i <- [0..(n-1)], (row+i) < length grid, (col+i) < length (grid!!(row+i))]
right grid n row col = [grid!!row!!(col+i) | i <- [0..(n-1)], row < length grid, (col+i) < length (grid!!row)]
rightUp grid n row col = [grid!!(row-i)!!(col+i) | i <- [0..(n-1)], (row-i) >= 0, (col+i) < length (grid!!(row-i))]

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)