module HW where

factors :: Integral t => t -> [t]
factors n = [i | i <-[1..n], (mod n i) == 0]

-- perfects :: Integral t => t -> [t]
-- perfects n = [x | x <- [1..n], isPerfect x ]
--   where isPerfect num = sum (factors num) == num

perfects :: Integral t => t -> [t]
perfects n = [x | x <- [1..n], isPerfect x ]
  where isPerfect num = sum (init (factors num)) == num
