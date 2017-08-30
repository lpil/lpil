module SecretHandshake
  ( handshake
  ) where

import Control.Arrow ((>>>))
import Data.Maybe (mapMaybe)

handshake :: Int -> [String]
handshake n
  | n >= 16 = reverseHandshake n
  | otherwise = handshake n
  where
    handshake = binaryList >>> zip actions >>> mapMaybe pickStep
    reverseHandshake = flip rem 16 >>> handshake >>> reverse
    splitDigit (n, _) = divMod n 2
    binaryList = flip divMod 2 >>> iterate splitDigit >>> map snd >>> take 4
    actions = ["wink", "double blink", "close your eyes", "jump"]
    pickStep (action, 1) = Just action
    pickStep _ = Nothing
