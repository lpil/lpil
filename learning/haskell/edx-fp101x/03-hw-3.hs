module HW where

import Prelude hiding ((&&))

(&&) :: Bool -> Bool -> Bool

-- -- 1
-- -- Success
-- True && True = True
-- _    && _    = False

-- -- 2
-- -- Success
-- a && b = if a then if b then True else False else False

-- -- 3
-- -- Failure
-- a && b = if not (a) then not (b) else True

-- -- 4
-- -- Syntax error, if needs an else
-- a && b = if a then b

-- -- 5
-- -- Failure
-- a && b = if a then if b then False else True else False

-- -- 6
-- -- Success
-- a && b = if a then b else False

-- 7
-- Success
a && b = if b then a else False

--
-- Test
--
main :: IO ()
main = print [
             False && False == False,
             False && True  == False,
             True  && False == False,
             True  && True  == True
             ]
