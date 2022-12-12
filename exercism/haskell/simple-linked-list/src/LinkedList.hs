module LinkedList
  ( LinkedList
  , datum
  , fromList
  , isNil
  , new
  , next
  , nil
  , reverseLinkedList
  , toList
  ) where

import Control.Arrow

data LinkedList a
  = Cons a
         (LinkedList a)
  | Nil
  deriving (Eq, Show)

-- Unsafe
datum :: LinkedList a -> a
datum Nil        = error "head of empty list"
datum (Cons a _) = a

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

-- Unsafe
next :: LinkedList a -> LinkedList a
next Nil         = error "tail of empty list"
next (Cons _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
 where
  go acc Nil         = acc
  go acc (Cons x xs) = go (Cons x acc) xs

toList :: LinkedList a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs
