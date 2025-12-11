findInList :: (Real t) => t -> [t] -> t
findInList x l = findInList' x l 0
  where
    findInList' x (h : t) depth
      | x == h = depth
      | null t = -1
      | otherwise = findInList' x t (depth + 1)

getDecimal :: (Real a) => a -> a
getDecimal n
  | x > 1 = x - 1
  | otherwise = x
  where x = n * 2

func (h:t) d
  | d == 0 = h
  | x == -1 = func (getDecimal h : h:t) (d-1)
  | otherwise = x
  where x = findInList (getDecimal h) (h:t)
