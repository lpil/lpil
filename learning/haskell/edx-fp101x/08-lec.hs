module LectureEight where

import System.IO

-- Hangman

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

sgetLn :: IO String
sgetLn = do x <- getCh
            if x == '\n'
              then do putChar x
                      return []
              else do putChar '-'
                      xs <- sgetLn
                      return (x:xs)

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word
                  then putStrLn "You got it!"
                  else do putStrLn (diff word xs)
                          guess word

diff :: String -> String -> String
diff xs ys = [ if x `elem` ys then x else '-' | x <- xs ]

hangman :: IO ()
hangman = do word <- sgetLn
             guess word
