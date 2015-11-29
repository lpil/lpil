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
      song 2 `shouldBe` "\
        \One man went to mow\n\
        \Went to mow a meadow\n\
        \One man and his dog\n\
        \Went to mow a meadow\n\n\

        \Two men went to mow\n\
        \Went to mow a meadow\n\
        \Two men, one man and his dog\n\
        \Went to mow a meadow\n"

    it "gets verse 1..3 correct" $
      song 3 `shouldBe` "\
        \One man went to mow\n\
        \Went to mow a meadow\n\
        \One man and his dog\n\
        \Went to mow a meadow\n\n\

        \Two men went to mow\n\
        \Went to mow a meadow\n\
        \Two men, one man and his dog\n\
        \Went to mow a meadow\n\n\

        \Three men went to mow\n\
        \Went to mow a meadow\n\
        \Three men, two men, one man and his dog\n\
        \Went to mow a meadow\n"
