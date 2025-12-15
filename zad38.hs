addToList :: (Integral a) => [a] -> a -> [a]
addToList [] v = [v]
addToList (h : t) v
  | v <= h = v : h : t
  | null t = [h, v]
  | otherwise = h : addToList t v

prefixlessSum :: (Integral a) => [a] -> a -> a
prefixlessSum l 0 = sum l
prefixlessSum (h : t) n = prefixlessSum (addToList (addToList t (h + 1)) (h + 4)) (n - 1)

cost :: (Integral a) => a -> a
cost 1 = 1
cost n = prefixlessSum [0] (n - 1)