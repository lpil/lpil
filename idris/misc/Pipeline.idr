module Pipeline

infixl 9 |>
infixr 0 <|

%access public export


||| Pipeline style function application
(|>) : a -> (a -> b) -> b
a |> f = f a


||| Backwards pipeline style function application
(<|) : (a -> b) -> a -> b
f <| a = f a
