import System.Environment

main = do
    args <- getArgs
    let n = read (head args) :: Int
    print (foldr lcm 1 (reverse [1..n]))
