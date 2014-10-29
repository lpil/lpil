import Prelude hiding ((||))

(||) :: Bool -> Bool -> Bool

-- -- 1
-- -- Success
-- False || False = False
-- _     || _     = True

-- -- 2
-- -- Success
-- False || b = b
-- True  || _ = True

-- -- 3
-- -- Failure
-- b || c
--   | b == c    = True
--   | otherwise = False

-- -- 4
-- -- Success
-- b || c
--   | b == c    = b
--   | otherwise = True

-- -- 5
-- -- Success
-- b || False = b
-- _ || True  = True

-- -- 6
-- -- Success
-- b || c
--   | b == c    = c
--   | otherwise = True

-- -- 7
-- -- Failure, non exhaustive patterns
-- b || True = b
-- _ || True = True

-- 8
-- Success
False || False = False
False || True  = True
True  || False = True
True  || True  = True

--
-- Test
--
main :: IO ()
main = print [
             False || False == False,
             False || True  == True,
             True  || False == True,
             True  || True  == True
             ]
