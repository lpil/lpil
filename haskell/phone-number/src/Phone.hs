module Phone
  ( number
  ) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  case drop1 $ filter isDigit xs of
    number@[_, _, _, area, _, _, exchange, _, _, _]
      | non1Digit area && non1Digit exchange -> Just number
    _ -> Nothing
  where
    drop1 ('1':xs) = xs
    drop1 xs = xs
    non1Digit '2' = True
    non1Digit '3' = True
    non1Digit '4' = True
    non1Digit '5' = True
    non1Digit '6' = True
    non1Digit '7' = True
    non1Digit '8' = True
    non1Digit '9' = True
    non1Digit _ = False
