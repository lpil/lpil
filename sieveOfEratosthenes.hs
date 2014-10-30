module SieveOfEratosthenes where

sieveOfEratosthenes :: [Integer]
sieveOfEratosthenes = sieve []
  where
    sieve :: Integral t => [t] -> [t]
    sieve []     = sieve [2..]
    sieve (x:xs) = x : sieve (filter (\y -> mod y x /= 0) xs)
