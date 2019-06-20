module HiAgainWorld where

import Data.Function ((&))


inc :: Num n => n -> n
inc x =
  x + 1


main :: IO ()
main =
  0
  & iterate inc
  & map show
  & map putStrLn
  & take 50
  & sequence_
