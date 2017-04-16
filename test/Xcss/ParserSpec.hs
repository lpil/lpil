{-# LANGUAGE OverloadedStrings #-}

module Xcss.ParserSpec
  ( main
  , spec
  ) where

import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck
import Xcss.Parser

main :: IO ()
main = hspec spec

p :: Text -> Either CssParseError [Statement]
p = parseCss "file.css"

spec :: Spec
spec =
  describe "parseCss" $ do
    it "discards whitespace" $ do
      p "" `shouldBe` Right []
      p "   " `shouldBe` Right []
      p " \t " `shouldBe` Right []
      p " \n " `shouldBe` Right []

    it "discards empty element blocks" $ do
      p "a{}" `shouldBe` Right []
      p "div {}" `shouldBe` Right []
      p ".title {}" `shouldBe` Right []
      p "#title {}" `shouldBe` Right []
      p "foo-bar {}" `shouldBe` Right []
      p "foo_bar {}" `shouldBe` Right []
      p "article {} h1 {}" `shouldBe` Right []
      p "#app .column ul li {}" `shouldBe` Right []
    it "discards element blocks" $ do
      p "a { flex: 1; }" `shouldBe` Right []
      p "a { color: red }" `shouldBe` Right []
      p "a { color: red; }" `shouldBe` Right []
      p "a { --size: 100; }" `shouldBe` Right []
      p "a { font-size: 16px; }" `shouldBe` Right []
      p "a { flex-grow: initial; }" `shouldBe` Right []
      p "a { content: 'Hello, sailor!'; }" `shouldBe` Right []
      p "a { color: red; display: hidden }" `shouldBe` Right []
      p "a { content: \"Hello, sailor!\"; }" `shouldBe` Right []
