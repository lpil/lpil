module Data.Stylesheet
  ( Ast(..)
  , parse
  ) where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign (Foreign)
import Data.Either (Either(..))

-- https://leanpub.com/purescript/read#leanpub-auto-using-javascript-code-from-purescript

data Ast
  = Function { name :: String }

derive instance genericAst :: Generic Ast

instance eqAst :: Eq Ast where
  eq = gEq

instance showAst :: Show Ast where
  show = gShow

parse :: String -> Either (Array Ast) String
parse source = do
  -- Tum tee tum, no parsing yet...
  let _ = postCssParse source
  Left []

-- TODO: Handle exceptions with Except
foreign import postCssParse :: String -> Foreign
