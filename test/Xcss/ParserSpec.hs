module Xcss.ParserSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Xcss.Parser

main :: IO ()
main = hspec spec

doParse :: String -> Either CssParseError Statement
doParse = parseCss "file.css"

spec :: Spec
spec =
  describe "parseCss" $ do
    it "parses whitespace only source" $ do
      doParse "" `shouldBe` Right (Statements [])
      doParse "   " `shouldBe` Right (Statements [])
      doParse " \t " `shouldBe` Right (Statements [])
      doParse " \n " `shouldBe` Right (Statements [])
    it "parses empty element blocks" $ do
      doParse "div {}" `shouldBe` Right (Statements [Block "div"])
      doParse "div {} div {}" `shouldBe`
        Right (Statements [Block "div", Block "div"])
      doParse "article {} h1 {}" `shouldBe`
        Right (Statements [Block "article", Block "h1"])
