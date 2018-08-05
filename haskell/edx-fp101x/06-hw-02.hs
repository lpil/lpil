module HW where

import Prelude hiding (any)

any :: (a -> Bool) -> [a] -> Bool

-- -- incorrect
-- any p = map p . or

any p = or . map p

-- any p xs = length (filter p xs) > 0

-- any p = not . null . dropWhile (not . p)

-- -- incorrect
-- any p = null . filter p

-- any p xs = not ( all (\x -> not (p x)) xs)

-- any p xs = foldr (\x acc -> (p x) || acc) False xs

-- -- Incorrect
-- any p xs = foldr (||) True (map p xs)
