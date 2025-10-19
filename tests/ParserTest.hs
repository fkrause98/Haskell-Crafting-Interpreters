module Main (main) where

import Test.Hspec
import Parsers
import Tokens
import Data.Either (isLeft)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Lox Parser" $ do
    -- Test for basic token types
    describe "Token Parsing" $ do
        it "parses an empty string as no tokens" $
            doParse "" `shouldBe` Right []

        it "ignores whitespace and comments" $
            doParse " \n // this is a comment \n " `shouldBe` Right []

        it "parses single-character tokens" $ do
            let input = "( ) { } , . - + ; * /"
            let expected = Right
                  [ LoxToken LeftParen  Nothing Nothing
                  , LoxToken RightParen Nothing Nothing
                  , LoxToken LeftBrace  Nothing Nothing
                  , LoxToken RightBrace Nothing Nothing
                  , LoxToken Comma      Nothing Nothing
                  , LoxToken Dot        Nothing Nothing
                  , LoxToken Minus      Nothing Nothing
                  , LoxToken Plus       Nothing Nothing
                  , LoxToken SemiColon  Nothing Nothing
                  , LoxToken Star       Nothing Nothing
                  , LoxToken Slash      Nothing Nothing
                  ]
            doParse input `shouldBe` expected

        it "parses multi-character tokens" $ do
            let input = "!= == >= <= ! = > <"
            let expected = Right
                  [ LoxToken BangEqual    Nothing Nothing
                  , LoxToken EqualEqual   Nothing Nothing
                  , LoxToken GreaterEqual Nothing Nothing
                  , LoxToken LessEqual    Nothing Nothing
                  , LoxToken Bang         Nothing Nothing
                  , LoxToken Equal        Nothing Nothing
                  , LoxToken Greater      Nothing Nothing
                  , LoxToken Less         Nothing Nothing
                  ]
            doParse input `shouldBe` expected

    -- Test for literals (strings, numbers, identifiers)
    describe "Literal Parsing" $ do
        it "parses a number literal" $
            doParse "123.45" `shouldBe` Right [LoxToken Number (Just "123.45") (Just (Num 123.45))]

        it "parses a string literal with spaces" $
            doParse "\"hello world!\"" `shouldBe` Right [LoxToken String (Just "hello world!") (Just (Str "hello world!"))]

        it "fails on an unterminated string" $
            doParse "\"hello" `shouldSatisfy` isLeft

        it "parses a simple identifier" $
            doParse "foo" `shouldBe` Right [LoxToken Identifier (Just "foo") Nothing]

    -- Test for keywords and a combined expression
    describe "Keyword and Expression Parsing" $ do
        it "parses keywords correctly" $ do
            let input = "var fun if else"
            let expected = Right
                  [ LoxToken Var  (Just "var")  Nothing
                  , LoxToken Fun  (Just "fun")  Nothing
                  , LoxToken If   (Just "if")   Nothing
                  , LoxToken Else (Just "else") Nothing
                  ]
            doParse input `shouldBe` expected
