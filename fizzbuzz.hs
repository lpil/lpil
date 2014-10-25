fizzbuzz :: (Show a, Integral a) => a -> [Char]
fizzbuzz x = unlines $ map fb [1..x]
  where fb x
          | mod x 15 == 0 = "FizzBuzz"
          | mod x  5 == 0 = "Fizz"
          | mod x  3 == 0 = "Buzz"
          | otherwise     = show x
