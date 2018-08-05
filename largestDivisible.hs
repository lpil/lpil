module LargestDivisible where

largestDivisible :: (Integral a) => a -> a -> a
largestDivisible x divider 
  | divider > x = -1
  | otherwise   = head $ filter divisable [x, (x - 1)..x]
      where
        divisable y = mod y divider == 0
