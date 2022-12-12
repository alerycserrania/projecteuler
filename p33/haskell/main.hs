toLct (num, den) = (div num (gcd num den), div den (gcd num den))
fractions = [((n*10+p, d+p*10), (n, d)) | n <- [0..9], d <- [1..9], p <- [1..9], p /= n]
fractionsLowest = map (\(f1, f2) -> (toLct f1, toLct f2))
solution = toLct (product (map fst fr), product (map snd fr))
  where fr = map (fst . fst) . filter (uncurry (==) . fst) $ (zip (fractionsLowest fractions) fractions)
