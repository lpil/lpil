module LectureTen where

-- Countdown solver!

-- Algebraic data type for mathematical operators
data Op = Add
        | Sub
        | Mul
        | Div

-- Recursive algebraic data type for expressions
data Expr = Val Int
          | App Op Expr Expr


-- Apply an operator to two integers to get a result
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Check if an expression is valid according to the countdown rules
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- Evaluate expressions
--    Using a list to handle no result rather than the Maybe type
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l  -- eval left side
                                , y <- eval r  -- eval right side
                                , valid o x y] -- only allow valid exps

-- choices [1,2] = [[],[1],[2],[1,2],[2,1]]
choices :: [a] -> [[a]]
choices = undefined

-- Gets all the values at the leafs
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- Decide if expression is a solution for a given list of numbers and
-- a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = eval e == [n] -- Check that it evals to target
                  &&
                  elem (values e) (choices ns) -- Check that the values used in
                                               -- in the expression is in
                                               -- the list of possible
                                               -- choices for the given numbers
                                               
-- split [1,2,3] = [([1],[2,3]), ([1,2],[3])]
split :: [a] -> [([a],[a])]
split = loop []
  where
    loop _ []      = []
    loop _ [_]     = []
    loop ys (z:zs) = (ys ++ [z], zs) : loop (ys ++ [z]) zs

-- Return a list of all possible expressions whose values are precisely
-- a given list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]

-- Combines two expressions using each operator
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

-- Returns a list of all possible expressions that solve an instance of the
-- countdown problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e   <- exprs ns'
                    , eval e == [n]]

