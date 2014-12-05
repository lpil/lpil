module SieveOfEratosthenes where

sieveOfEratosthenes :: [Integer]
sieveOfEratosthenes = sieve []
  where
    sieve :: Integral t => [t] -> [t]
    sieve []     = sieve [2..]
    sieve (x:xs) = x : sieve (filter (\y -> mod y x /= 0) xs)


sieveOfEratosthenes2 :: [Integer]
sieveOfEratosthenes2 = sieve []
  where
    doesntDivide x y = mod y x /= 0

    sieve :: Integral t => [t] -> [t]
    sieve []     = sieve [2..]
    sieve (x:xs) = x : sieve (filter (doesntDivide x) xs)


sieveOfEratosthenes3 :: [Integer]
sieveOfEratosthenes3 = sieve []
  where
    sieve :: Integral t => [t] -> [t]
    sieve []     = sieve [2..]
    sieve (p:xs) = p : sieve [x | x <- xs
                                , mod x p /= 0]
