main = do
  print (last . dynRec [1,2,5,10,20,50,100,200] $ 200)


dynRec ps m = dynRec' ps [] (1 : replicate m 0) 
  where 
    dynRec' [] _ old = old
    dynRec' (p:ps') new old = dynRec' ps' [] (dynamic new old p)

dynamic new [] _ = new
dynamic new old v = dynamic nnew (tail old) v
  where vnew = if (v-1) < (length new) then ((reverse new)!!(v-1)) else 0
        nnew = new ++ [vnew + (head old)]