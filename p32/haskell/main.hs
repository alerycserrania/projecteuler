import Data.Char
import Data.List
import qualified Data.Set

digits ns = sum . Data.Set.fromList 
                . map (\(_,_,p) -> p) 
                . filter ((==9) . length . flt) 
                . filter (isPandigital . flt) 
                . map ao $ spl
  where
    flt (l, r, p) = (show l) ++ (show r) ++ (show p)
    spl = (allSplits ns)
    ao (lsp, rsp) = (l, r, l * r)
      where l = toNum (lsp)
            r = toNum (rsp)

toNum ns = sum . map (\(p, d) -> d * 10 ^ p) $ zip [0..] (reverse ns)
allSplits ls = concat . map (\i -> splits i ls) $ [2..5] 
splits n ls = [(splitAt i cb) | cb <- combinations n ls, i <- init . map fst . zip [1..] $ cb]
combinations n ls = concat . map permutations $ (sub n ls)
sub n ls =  filter ((==n) . length) (subsequences ls)


popAt xs i = (xs!!i, f ++ (tail s)) 
  where (f, s) = splitAt i xs

isPandigital n = (Data.Set.fromList n) == ref && (length n) == (length ref)
  where
    ref = Data.Set.fromList (map (intToDigit) [1..(length n)])