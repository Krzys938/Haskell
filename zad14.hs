reversal :: (Integral a) => a -> a
reversal a = reversal' 0 a
  where
    reversal' a 0 = a
    reversal' a b = let (q, r) = b `divMod` 10 in reversal' (a * 10 + r) q

isPalindrome :: (Integral a) => a -> Bool
isPalindrome x = reversal x == x


lychrel :: (Integral t) => t -> t -> t
lychrel n x
  | isPalindrome x = x
  | n > 0 = lychrel (n - 1) (x + reversal x)
  | otherwise = -1