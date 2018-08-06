-- This module contains a list of all English words, as adapted from
-- http://www-01.sil.org/linguistics/wordlists/english/
--
-- To use, say `import Words` after your module header line and then
-- access the list of words named `allWords`.

module Words where

import FileText

scrabbleValue :: Char -> Int
scrabbleValue 'a' = 1
scrabbleValue 'b' = 3
scrabbleValue 'c' = 3
scrabbleValue 'd' = 2
scrabbleValue 'e' = 1
scrabbleValue 'f' = 4
scrabbleValue 'g' = 2
scrabbleValue 'h' = 4
scrabbleValue 'i' = 1
scrabbleValue 'j' = 8
scrabbleValue 'k' = 5
scrabbleValue 'l' = 1
scrabbleValue 'm' = 3
scrabbleValue 'n' = 1
scrabbleValue 'o' = 1
scrabbleValue 'p' = 3
scrabbleValue 'q' = 10
scrabbleValue 'r' = 1
scrabbleValue 's' = 1
scrabbleValue 't' = 1
scrabbleValue 'u' = 1
scrabbleValue 'v' = 4
scrabbleValue 'w' = 4
scrabbleValue 'x' = 8
scrabbleValue 'y' = 4
scrabbleValue 'z' = 10
scrabbleValue _ = minBound  -- shouldn't happen

allWords :: [String]
allWords = lines $(fileText "words.txt")
