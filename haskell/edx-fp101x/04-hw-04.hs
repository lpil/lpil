module HW where

correct :: [(Integer, Integer)]
correct = [(x, y) | x <- [1,2,3], y <- [3,5,6] ]

-- f1 :: [(Integer, Integer)]
-- f1 = [z | z <- [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]]

-- f2 :: [(Integer, Integer)]
-- f2 = concat [[[(x,y)] | x <- [1,2,3]] | y <- [4,5,6]]

-- f3 :: [(Integer, Integer)]
-- f3 = concat [(x,y) | y <- [4,5,6]] | x <- [1,2,3]

f4 :: [(Integer, Integer)]
f4 = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
