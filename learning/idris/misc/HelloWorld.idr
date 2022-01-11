module Main

main : IO ()
-- main = putStrLn "Hello, world!"
-- main = putStrLn ?greeting
-- main = putStrLn 'x'
-- main = putStrLn (?convert 'x')
main = putStrLn (cast 'x')
