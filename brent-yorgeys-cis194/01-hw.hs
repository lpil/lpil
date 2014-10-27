-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf

module HW01 where         -- We'll learn more about this later

--
-- Validate credit card numbers
--

-- In this section, you will implement the validation algorithm for credit
-- cards. It follows these steps:

-- Double the value of every second digit beginning from the right. That
-- is, the last digit is unchanged; the second-to-last digit is dou- bled;
-- the third-to-last digit is unchanged; and so on. For example, [1,3,8,6]
-- becomes [2,3,16,6]

-- Add the digits of the doubled values and the undoubled dig- its from the
-- original number. For example, [2,3,16,6] becomes 2+3+1+6+6 = 18

-- Calculate the remainder when the sum is divided by 10 For the above
-- example, the remainder would be 8

-- If the result equals 0, then the number is valid.

--
-- Exercise 1
--

-- We first need to be able to break up a number into its last digit and
-- the rest of the number. Write these functions:

-- lastDigit :: Integer -> Integer
-- dropLastDigit :: Integer -> Integer

-- lastDigit 123 == 3
-- lastDigit 0 == 0

-- dropLastDigit 123 == 12
-- dropLastDigit 5 == 0

lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

--
-- Exercise 2
--

-- Now, we can break apart a number into its digits. Define the function

-- toDigits :: Integer -> [Integer]

-- toDigits should convert positive Integers to a list of digits.
-- (For 0 or negative inputs, toDigits should return the empty list.)

-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []

-- How do I do this without reversing?
toDigits :: Integer -> [Integer]
toDigits n
  | n < 1     = []
  | otherwise = reverse $ split [] n
    where split _   0 = []
          split acc m = lastDigit m : split acc (dropLastDigit m)

--
-- Exercise 3
--

-- Once we have the digits in the proper order, we need to double every
-- other one. Define a function

-- doubleEveryOther :: [Integer] -> [Integer]

-- Remember that doubleEveryOther should double every other number
-- beginning from the right, that is, the second-to-last, fourth-to-last,
-- ... numbers are doubled.

-- Note that it’s much easier to perform this operation on a list of digits
-- that’s in reverse order. You will likely need helper functions to make
-- this work.

-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ calc xs
  where calc []        = []
        calc (y:[])    = [y * 2]
        calc (y:y2:ys) = calc ys ++ [y2, y * 2]

--
-- Exercise 4
--

-- The output of doubleEveryOther has a mix of one-digit and two-digit
-- numbers. Define the function

-- sumDigits :: [Integer] -> Integer

-- to calculate the sum of all digits.

-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits = foldl calc 0
  where calc acc x
          | x < 10    = acc + x
          | otherwise = acc + lastDigit x + dropLastDigit x
