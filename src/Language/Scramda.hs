module Language.Scramda
  ( Expr(..)
  , freeIn
  ) where

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Var s) = s
  show (Lam x e) = x ++ " -> " ++ show e
  show (App f x) = showF f ++ " " ++ showX x
    where
      showF f@(Lam _ _) = "(" ++ show f ++ ")"
      showF f = show f
      showX x@(Lam _ _) = "(" ++ show x ++ ")"
      showX x@(App _ _) = "(" ++ show x ++ ")"
      showX x = show x

infix 7 `freeIn`
freeIn :: String -> Expr -> Bool
var `freeIn` (Var x) = var == x
var `freeIn` (Lam x e) = var /= x && var `freeIn` e
var `freeIn` (App f x) = var `freeIn` f || var `freeIn` x