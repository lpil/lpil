module Data.Context
  ( Context
  , textContext
  ) where

type Context =
  { text :: String }

textContext :: String -> Context
textContext text =
  { text }
