import Data.Set

main = do
  print (length . Data.Set.fromList $ gen [2..100] [2..100])

gen [] _ = []
gen (h:hs) xs = scanl (\n _ -> n*h) (h^(head xs)) (tail xs) ++ gen hs xs