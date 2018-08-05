module HW where

import Prelude hiding (all)

all :: (a -> Bool) -> [a] -> Bool

-- all p xs = and (map p xs)

-- -- Incorrect
-- all p xs = map p (and xs)

-- all p = and . map p

-- all p = not . any (not . p)

-- -- Incorrect
-- all p = map p . and

-- all p xs = foldl (&&) True (map p xs)

-- -- Incorrect
-- all p xs = foldl (&&) False (map p xs)

all p = foldr (&&) True . map p
