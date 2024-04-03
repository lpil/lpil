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


counts_repl : IO ()
counts_repl =
  repl "\nI'll count the letters and words\n" (show . counts)


||| Write a top_ten function that returns the ten
||| largest values in a list.
|||
top_ten : Ord a => List a -> List a
top_ten =
  List.take 10 . List.reverse . List.sort


||| Write an over_length function that returns the
||| number of strings in the list longer than the
||| given number of characters.
|||
over_length : Nat -> List String -> Nat
over_length minLen strings =
  List.length
  . filter (\x => (length x) > minLen)
  $ strings
