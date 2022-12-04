main = do
    print (sum . filter (pOr [mul 3, mul 5]) $ [1..999])

mul m d = (mod d m) == 0

pOr [] _ = False
pOr (p:ps) x = (p x) || (pOr ps x)

