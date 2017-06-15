module Triangle
  ( TriangleType(..)
  , triangleType
  ) where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType
  :: (Eq a, Ord a, Num a)
  => a -> a -> a -> TriangleType
triangleType a b c
  | any (<= 0) [a, b, c] = Illegal
  | a == b && b == c = Equilateral
  | a + b < c = Illegal
  | b + c < a = Illegal
  | c + a < b = Illegal
  | a == b = Isosceles
  | b == c = Isosceles
  | c == a = Isosceles
  | otherwise = Scalene
