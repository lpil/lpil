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

toDigits :: Integer -> [Integer]
toDigits n
  | n < 1     = []
  | otherwise = reverse $ split [] n
    where split _   0 = []
          split acc m = lastDigit m : split acc (dropLastDigit m)
-- How do I do this without reversing?
