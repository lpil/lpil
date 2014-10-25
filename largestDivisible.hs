largestDivisible :: (Integral a) => a -> a -> a
largestDivisible x divider 
  | divider > x = -1
  | otherwise   = head $ filter pred [x, (x - 1)..x]
      where pred y = mod y divider == 0
