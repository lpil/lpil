quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quicksort $ filter (<= x) xs
        bigger  = quicksort $ filter (>  x) xs
