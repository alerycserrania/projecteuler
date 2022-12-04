main = do
  print (head . dropWhile limit . scanl next (0, 1, 0) $ [1..])

limit (u,v,_) = u+v < 4_000_000
next (u, v, s) _
  | mod (u+v) 2 == 0 = (v, u + v, s + u + v)
  | otherwise = (v, u + v, s)