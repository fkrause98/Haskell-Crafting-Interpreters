module Expression where

import Tokens (LoxToken)

data LiteralValue where
  LitNum :: Float -> LiteralValue
  LitStr :: String -> LiteralValue
  LitBool :: Bool -> LiteralValue
  LitNil :: LiteralValue
  deriving (Eq, Show)

data Expr where
  LiteralExpr :: {litValue :: LiteralValue} -> Expr
  UnaryExpr :: {unaryOperator :: LoxToken, unaryRight :: Expr} ->
                 Expr
  BinaryExpr :: {binaryLeft :: Expr,
                   binaryOperator :: LoxToken,
                   binaryRight :: Expr} ->
                  Expr
  GroupingExpr :: {groupingExpr :: Expr} -> Expr
  deriving (Eq, Show)
