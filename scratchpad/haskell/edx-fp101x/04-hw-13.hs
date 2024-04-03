module HW where

divides :: Int -> Int -> Bool
divides n divisor = mod n divisor == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]
