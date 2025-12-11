addToList :: (Integral a) => [a] -> a -> [a]
addToList [] v = [v]
addToList (h : t) v
  | v <= h = v : h : t
  | null t = [h, v]
  | otherwise = h : addToList t v

prefixlessSum :: (Integral a) => [a] -> a -> a
prefixlessSum (h : t) n
  | n == 0 = sum (h : t)
  | otherwise = prefixlessSum (addToList (addToList t (h + 1)) (h + 4)) (n - 1)

prefixlessCode :: (Integral a) => a -> a
prefixlessCode 1 = 1
prefixlessCode n = prefixlessSum [0] (n - 1)