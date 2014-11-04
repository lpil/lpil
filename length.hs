module Length where

length2 :: Integral b => [a] -> b
length2 = foldl count 0
  where count acc _ = acc + 1
