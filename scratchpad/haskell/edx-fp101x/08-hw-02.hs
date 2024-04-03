module HW where

putStr' :: String -> IO ()
putStr' = foldr ((>>) . putChar) (return ())

putStrLn' :: String -> IO ()

-- -- CORRECT
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putStrLn' ""

-- -- CORRECT
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putStrLn' ""

-- -- CORRECT
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >>= \_ -> putChar '\n'

-- -- Type error
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >>= \_ -> putChar '\n'

-- CORRECT
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStr' "\n"

-- -- Inf loop of newlines
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putStrLn' "\n"

-- -- Type error, returns Char
-- putStrLn' [] = return ""
-- putStrLn' xs = putStrLn' xs >> putStr' "\n"

-- -- Lets not even go any further, this is a type error
-- putStrLn' [] = putChar "\n"

