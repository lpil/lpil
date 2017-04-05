module TDDExercises

palindrome : String -> Bool
palindrome s =
  s == (reverse s)


longerPalindrome : Nat -> String -> Bool
longerPalindrome len s =
  (length s) > len
  && (reverse s) == s


||| Write a counts function that returns a pair of the
||| number of words in the input and the number of
||| characters in the input.
|||
counts : String -> (Nat, Nat)
counts input =
  (length (words input), length input)



||| Write a top_ten function that returns the ten
||| largest values in a list.
|||
top_ten : Ord a => List a -> List a
top_ten =
  List.take 10 . List.reverse . List.sort
