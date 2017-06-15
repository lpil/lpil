module Beer
  ( song
  ) where

import Data.List (intercalate)

song :: String
song = intercalate "\n" $ map verse [99,98 .. 0]

verse :: Int -> String
verse 0 =
  unlines
    [ "No more bottles of beer on the wall, no more bottles of beer."
    , "Go to the store and buy some more, 99 bottles of beer on the wall."
    ]
verse 1 =
  unlines
    [ "1 bottle of beer on the wall, 1 bottle of beer."
    , "Take it down and pass it around, no more bottles of beer on the wall."
    ]
verse 2 =
  unlines
    [ "2 bottles of beer on the wall, 2 bottles of beer."
    , "Take one down and pass it around, 1 bottle of beer on the wall."
    ]
verse n =
  unlines
    [ show n ++
      " bottles of beer on the wall, " ++ show n ++ " bottles of beer."
    , "Take one down and pass it around, " ++
      show (n - 1) ++ " bottles of beer on the wall."
    ]
