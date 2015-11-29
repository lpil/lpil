module Chapter01Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Chapter01
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "exercise D" $ do
    specify "an alternate method" $ property $
      \str -> let
        example   = words . map toLower
        alternate = map (map toLower) . words
      in
        example str == alternate str

  describe "exercise G: song" $ do
    it "is nothing when < 0" $
      song (-1) `shouldBe` ""

    it "is nothing when = 0" $
      song 0 `shouldBe` ""

    it "gets verse 1 correct" $
      song 1 `shouldBe` "\
        \One man went to mow\n\
        \Went to mow a meadow\n\
        \One man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..2 correct" $
      checkVerse 2 "\n\
        \Two men went to mow\n\
        \Went to mow a meadow\n\
        \Two men, one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..3 correct" $
      checkVerse 3 "\n\
        \Three men went to mow\n\
        \Went to mow a meadow\n\
        \Three men, two men, one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..4 correct" $
      checkVerse 4 "\n\
        \Four men went to mow\n\
        \Went to mow a meadow\n\
        \Four men, three men, two men, one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..5 correct" $
      checkVerse 5 "\n\
        \Five men went to mow\n\
        \Went to mow a meadow\n\
        \Five men, four men, three men, two men, one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..6 correct" $
      checkVerse 6 "\n\
        \Six men went to mow\n\
        \Went to mow a meadow\n\
        \Six men, five men, four men, three men, two men, \
        \one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..7 correct" $
      checkVerse 7 "\n\
        \Seven men went to mow\n\
        \Went to mow a meadow\n\
        \Seven men, six men, five men, four men, three men, two men, \
        \one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..8 correct" $
      checkVerse 8 "\n\
        \Eight men went to mow\n\
        \Went to mow a meadow\n\
        \Eight men, seven men, six men, five men, four men, three men, \
        \two men, one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..9 correct" $
      checkVerse 9 "\n\
        \Nine men went to mow\n\
        \Went to mow a meadow\n\
        \Nine men, eight men, seven men, six men, five men, four men, \
        \three men, two men, one man and his dog\n\
        \Went to mow a meadow\n"


dropPrefix :: String -> String -> String
dropPrefix "" x = x
dropPrefix (x:xs) (y:ys)
  | x == y    = dropPrefix xs ys
  | otherwise = error "Not a prefix"

checkVerse n expected =
  let
    verseN = dropPrefix (song $ n - 1) $ song n
  in
    verseN `shouldBe` expected
