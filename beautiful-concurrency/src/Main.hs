module Main where

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

type Account = TVar Int

{-| Because STM is a different type to IO we cannot access
   the account without making use of the `atomically` function,
   so access to this data is safe.
-}
withdraw :: Account -> Int -> STM ()
withdraw account amount = do
  balence <- readTVar account
  let newBalence = balence - amount
  check $ newBalence >= 0
  writeTVar account newBalence

deposit :: Account -> Int -> STM ()
deposit account amount = withdraw account (-amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
  atomically $ do
    deposit to amount
    withdraw from amount

readAccount :: Account -> IO Int
readAccount account = atomically $ readTVar account

printAccount :: Account -> IO ()
printAccount account = do
  value <- readAccount account
  putStrLn $ show value

main :: IO ()
main = do
  account1 <- atomically $ newTVar 0
  account2 <- atomically $ newTVar 0
  printAccount account1
  printAccount account2
  forkIO $ transfer account1 account2 50
  forkIO $ atomically $ deposit account1 10
  forkIO $ atomically $ deposit account1 10
  forkIO $ atomically $ deposit account1 10
  forkIO $ atomically $ deposit account1 10
  forkIO $ atomically $ deposit account1 10
  atomically $ do
    amount <- readTVar account1
    check $ amount == 50
  printAccount account1
  printAccount account2
