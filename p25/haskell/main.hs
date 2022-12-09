

main = do
    print (fst . head . dropWhile ((1000>) . length . show . snd) . enumerate $ fibonacci)

enumerate = zip [1..]
fibonacci = map fst . scanl (\(u0, u1) _ -> (u1, u0 + u1)) (1, 1) $ [1..]