module Scramda
  ( Expr(..)
  ) where

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Eq, Show)