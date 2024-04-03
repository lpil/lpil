module Main

-- stringOrInt : Bool -> Type
-- stringOrInt x =
--   case x of
--     True => Int
--     False => String

stringOrInt : Bool -> Type
stringOrInt True = Int
stringOrInt False = String

getStringOrInt : (x : Bool) -> stringOrInt x
getStringOrInt x =
  case x of
    True => 94
    False => "Ninety four"

valToString : (x : Bool) -> stringOrInt x -> String
valToString x val =
  case x of
    True => cast val
    False => val

main : IO ()
main =
  putStrLn (valToString False "Hello")
-- main =
--   putStrLn (valToString True 1)
