module HW where

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 []     = []
safetail2 (_:xs) = xs

-- Non exhastive patterns
--
-- safetail3 :: [a] -> [a]
-- safetail3 (_ : xs)
--   | null xs   = []
--   | otherwise = tail xs

safetail4 :: [a] -> [a]
safetail4 xs
  | null xs = []
  | otherwise = tail xs

-- tail of empty list on empty list. Not safe
--
-- safetail5 :: [a] -> [a]
-- safetail5 xs = tail xs
-- safetail5 [] = []

safetail6 :: [a] -> [a]
safetail6 [] = []
safetail6 xs = tail xs

-- Non exhastive patterns
--
-- safetail7 :: [a] -> [a]
-- safetail7 [x]    = [x]
-- safetail7 (_:xs) = xs

safetail8 :: [a] -> [a]
safetail8
  = \ xs ->
     case xs of
       []     -> []
       (_:xs) -> xs
