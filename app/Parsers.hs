module Parsers where
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (lexeme)
import qualified Text.Megaparsec.Char.Lexer as L
import Tokens
import Data.Void



type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

doParse :: String -> Either ParserError [LoxToken]
doParse line = parse pTokens "" line

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

_lexeme :: Parser a -> Parser a
_lexeme = L.lexeme sc

pTokens :: Parser [LoxToken]
pTokens = sc *> many (_lexeme pSingleToken) <* eof
  where
    pSingleToken = choice [pToken, pString, pNumber, pIdentifier]

pToken :: Parser LoxToken
pToken = do
  pos <- getSourcePos

  tokenType <- ( choice [ char '(' >> return LeftParen,
                     char ')' >> return RightParen,
                     char '{' >> return LeftBrace,
                     char '}' >> return RightBrace,
                     char ',' >> return Comma,
                     char '.' >> return Dot,
                     char '-' >> return Minus,
                     char '+' >> return Plus,
                     char ';' >> return SemiColon,
                     char '+' >> return Plus,
                     char '*' >> return Star,
                     try (string "!=") >> return BangEqual,
                     try (string "==") >> return EqualEqual,
                     try (string ">=") >> return GreaterEqual,
                     try (string "<=") >> return LessEqual,
                     char ('!') >> return Bang,
                     char ('<') >> return Less,
                     char ('>') >> return Greater,
                     char ('=') >> return Equal,
                     char '/' >> notFollowedBy ( char '/') >> return Slash
                     ] )
  buildToken tokenType pos
  where
    buildToken tokenType pos = return LoxToken { tokenType = tokenType, lexeme = Nothing, literal = Nothing, position = pos}

pString :: Parser  LoxToken
pString = do
  pos <- getSourcePos
  str <- between (char '"') (char '"') ( many (noneOf "\"") )
  return LoxToken { tokenType = String, lexeme = Just str, literal = Just (Str str), position = pos }

pNumber :: Parser LoxToken
pNumber = do
  pos <- getSourcePos
  num <- float
  buildNumToken num pos
  where
    buildNumToken dec pos = return LoxToken { tokenType = Number, lexeme = Just $ show dec, literal = Just (Num dec), position = pos }

pComment :: Parser ()
pComment = skipLineComment "//"

pIdentifier :: Parser ( LoxToken )
pIdentifier = do
  pos <- getSourcePos
  identifier <- some alphaNumChar
  buildToken identifier pos
  where
    buildToken identifier pos = return LoxToken { tokenType = matchToken identifier, lexeme = Just identifier, literal = Nothing, position = pos }
    matchToken identifier
        | identifier == "and" = And
        | identifier == "class" = Class
        | identifier == "else" = Else
        | identifier == "false" = Tokens.False
        | identifier == "for" = For
        | identifier == "fun" = Fun
        | identifier == "if" = If
        | identifier == "nil" = Nil
        | identifier == "or" = Or
        | identifier == "print" = Print
        | identifier == "return" = Return
        | identifier == "super" = Super
        | identifier == "this" = This
        | identifier == "true" = Tokens.True
        | identifier == "var" = Var
        | identifier == "while" = While
        | otherwise = Identifier
