main = do
  let nbSundayFirst = length
                . filter ((==1) . day . fst)
                . filter ((==7) . snd) 
                . takeWhile (((>=)(Date 2000 12 31)) . fst) 
                . dropWhile (((>)(Date 1901 1 1)) . fst) 
                $ (zip (gen (Date 1900 1 1)) (cycle [1, 2, 3, 4, 5, 6, 7]))
  print (nbSundayFirst)


data Date = Date { year :: Int, month :: Int, day :: Int } deriving (Show, Eq, Ord)

gen date = date : gen (next date)

next date =
  if (nbDaysInMonth date) == day
    then if month == 12
      then (date { year = year + 1, month = 1, day = 1 })
      else (date { month = month + 1, day = 1 })
    else (date { day = day + 1})
  where (Date year month day) = date

nbDaysInMonth date 
  | elem (month date) [1, 3, 5, 7, 8, 10, 12] = 31
  | (month date) == 2 = if isLeapYear date then 29 else 28
  | otherwise = 30

isLeapYear date 
  | (mod (year date) 400 == 0) = True
  | (mod (year date) 100 == 0) = False
  | (mod (year date) 4 == 0) = True
  | otherwise = False 

