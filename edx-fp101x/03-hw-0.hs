-- halve1 :: [a] -> ([a], [a]) 
-- halve1 xs = (take n xs, drop n xs)
--   where n = length xs / 2

halve2 :: [a] -> ([a], [a]) 
halve2 xs = splitAt (length xs `div` 2) xs

halve3 :: [a] -> ([a], [a]) 
halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- halve4 :: [a] -> ([a], [a]) 
-- halve4 xs = splitAt (length xs `div` 2)

-- halve5 :: [a] -> ([a], [a]) 
-- halve5 xs = (take n xs, drop (n + 1) xs)
--   where n = length xs `div` 2

halve6 :: [a] -> ([a], [a]) 
halve6 xs = splitAt (div (length xs) 2) xs

-- halve7 :: [a] -> ([a], [a]) 
-- halve7 xs = splitAt (length xs / 2) xs

halve8 :: [a] -> ([a], [a]) 
halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2
