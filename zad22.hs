findInList :: (Integral t) => t -> [t] -> t
findInList x l = findInList' x l 0
  where
    findInList' x (h : t) depth
      | x == h = depth
      | null t = -1
      | otherwise = findInList' x t (depth + 1)

getRest n denomin = (n * 2) `mod` denomin

getPeriodLength (h : t) denomin
  | x == -1 = getPeriodLength (getRest h denomin : h : t) denomin
  | h == 0 = 0
  | otherwise = x + 1
  where
    x = findInList (getRest h denomin) (h : t)

getLongestPeriod n =
  map snd (filter (\(x, _) -> x == m) l)
  where
    l = map (\x -> (getPeriodLength [1] x, x)) [2 .. n]
    (m, _) = maximum l