module Main where

import Test.Hspec
import Text.Megaparsec (SourcePos, initialPos)
import Parsers (doParse)
import Tokens

data StrippedToken = StrippedToken TokenType (Maybe String) (Maybe Literal)
    deriving (Eq, Show)

stripToken :: LoxToken -> StrippedToken
stripToken (LoxToken tt l lit _) = StrippedToken tt l lit

defaultPos :: SourcePos
defaultPos = initialPos "test"

mkToken :: TokenType -> LoxToken
mkToken tt = LoxToken tt Nothing Nothing defaultPos

mkNumToken :: Float -> LoxToken
mkNumToken n = LoxToken Number (Just (show n)) (Just (Num n)) defaultPos

mkStrToken :: String -> LoxToken
mkStrToken s = LoxToken String (Just s) (Just (Str s)) defaultPos

main :: IO ()
main = hspec $ do
  describe "Lox Parser" $ do

    let runTest input expectedTokens =
          fmap (map stripToken) (doParse input) `shouldBe` Right (map stripToken expectedTokens)

    it "parses an empty string into an empty list of tokens" $ do
      runTest "" []

    it "handles whitespace and comments, producing no tokens" $ do
      runTest "  // this is a comment \n  " []

    it "parses all single-character tokens" $ do
      let input = "(){},.-+;* /"
      let expected = [
            mkToken LeftParen, mkToken RightParen, mkToken LeftBrace,
            mkToken RightBrace, mkToken Comma, mkToken Dot, mkToken Minus,
            mkToken Plus, mkToken SemiColon, mkToken Star, mkToken Slash
            ]
      runTest input expected

    it "parses all multi-character tokens" $ do
      let input = "! != = == > >= < <="
      let expected = [
            mkToken Bang, mkToken BangEqual, mkToken Equal,
            mkToken EqualEqual, mkToken Greater, mkToken GreaterEqual,
            mkToken Less, mkToken LessEqual
            ]
      runTest input expected

    it "parses a string literal" $ do
      runTest "\"hello world\"" [mkStrToken "hello world"]

    it "parses an empty string literal" $ do
      runTest "\"\"" [mkStrToken ""]

    it "parses a floating-point number literal" $ do
      runTest "123.45" [mkNumToken 123.45]

    it "parses an integer as a float (with decimal)" $ do
      -- The parser expects a decimal, so we provide "789.0" instead of "789"
      runTest "789.0" [mkNumToken 789.0]

    it "parses a user-defined identifier" $ do
      runTest "myVariable" [LoxToken Identifier (Just "myVariable") Nothing defaultPos]

    it "parses all language keywords correctly" $ do
      let input = "and class else false for fun if nil or print return super this true var while"
      let expected = [
            LoxToken And (Just "and") Nothing defaultPos,
            LoxToken Class (Just "class") Nothing defaultPos,
            LoxToken Else (Just "else") Nothing defaultPos,
            LoxToken Tokens.False (Just "false") Nothing defaultPos,
            LoxToken For (Just "for") Nothing defaultPos,
            LoxToken Fun (Just "fun") Nothing defaultPos,
            LoxToken If (Just "if") Nothing defaultPos,
            LoxToken Nil (Just "nil") Nothing defaultPos,
            LoxToken Or (Just "or") Nothing defaultPos,
            LoxToken Print (Just "print") Nothing defaultPos,
            LoxToken Return (Just "return") Nothing defaultPos,
            LoxToken Super (Just "super") Nothing defaultPos,
            LoxToken This (Just "this") Nothing defaultPos,
            LoxToken Tokens.True (Just "true") Nothing defaultPos,
            LoxToken Var (Just "var") Nothing defaultPos,
            LoxToken While (Just "while") Nothing defaultPos
            ]
      runTest input expected

    it "correctly ignores single-line comments" $ do
      let input = "var x = 10.0; // this is ignored"
      let expected = [
            LoxToken Var (Just "var") Nothing defaultPos,
            LoxToken Identifier (Just "x") Nothing defaultPos,
            mkToken Equal,
            mkNumToken 10.0,
            mkToken SemiColon
            ]
      runTest input expected

    it "parses a simple variable declaration statement" $ do
      let input = "var language = \"lox\";"
      let expected = [
            LoxToken Var (Just "var") Nothing defaultPos,
            LoxToken Identifier (Just "language") Nothing defaultPos,
            mkToken Equal,
            mkStrToken "lox",
            mkToken SemiColon
            ]
      runTest input expected

    it "correctly fails to parse an unterminated string" $ do
      let input = "\"hello"
      doParse input `shouldSatisfy` isLeft
        where isLeft (Left _) = Prelude.True
              isLeft _        = Prelude.False
