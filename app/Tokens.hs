module Tokens where
data TokenType
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | SemiColon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier
  | String
  | Number
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | EOF
  deriving (Eq, Show, Enum)

data Literal where
  Num :: (Float) -> Literal
  Str :: (String) -> Literal
  deriving (Eq, Show)

data LoxToken where
  LoxToken ::
    { tokenType :: TokenType,
      lexeme :: Maybe String,
      literal :: Maybe Literal
    } ->
    LoxToken
  deriving (Eq, Show)
