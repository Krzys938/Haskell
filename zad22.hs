findInList :: (Integral t) => t -> [t] -> t
findInList x l = findInList' x l 0
  where
    findInList' x (h : t) depth
      | x == h = depth
      | null t = -1
      | otherwise = findInList' x t (depth + 1)

getRest n denomin
  | x >= denomin = x - denomin
  | otherwise = x
  where x = n * 2

func (h:t) denomin
  | x == -1 = func (getRest h denomin : h:t) denomin
  | h == 0 = x
  | otherwise = x + 1
  where x = findInList (getRest h denomin) (h:t)
