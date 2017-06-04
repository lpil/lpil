module DNA
  ( toRNA
  ) where

toRNA :: String -> Maybe String
toRNA = mapM translateBase
  where
    translateBase 'G' = Just 'C'
    translateBase 'C' = Just 'G'
    translateBase 'T' = Just 'A'
    translateBase 'A' = Just 'U'
    translateBase _ = Nothing
