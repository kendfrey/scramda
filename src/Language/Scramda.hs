module Language.Scramda
  ( Expr(..)
  , freeIn
  , nf
  , substitute
  , toInt
  , toIntMaybe
  , toIO
  , whnf
  ) where

import Data.List
import Data.Maybe

data Expr = Var String
          | Lam String Expr
          | App Expr Expr
          | PrimFunc (Expr -> Maybe Expr)
          | PrimInt Int
          | PrimIO (IO Expr)

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
  show (PrimFunc _) = "{Primitive Function}"
  show (PrimInt x) = "{Primitive Int:" ++ show x ++ "}"
  show (PrimIO _) = "{Primitive IO}"

instance Eq Expr where
  (Var x) == (Var y) = x == y
  (Lam x e) == (Lam x' e') = x == x' && e == e'
  (App f x) == (App f' x') = f == f' && x == x'
  (PrimInt x) == (PrimInt y) = x == y
  _ == _ = False

infix 7 `freeIn`
freeIn :: String -> Expr -> Bool
var `freeIn` (Var x) = var == x
var `freeIn` (Lam x e) = var /= x && var `freeIn` e
var `freeIn` (App f x) = var `freeIn` f || var `freeIn` x
var `freeIn` _ = False

substitute :: String -> Expr -> Expr -> Expr
substitute var e v@(Var x) | var == x = e
                           | otherwise = v
substitute var e (Lam x e') = Lam x' . substitute var e . substitute x (Var x') $ e'
  where x' = head . filter (not . (`freeIn` e)) . map (x ++) . inits $ repeat '$'
substitute var e (App f x) = App (substitute var e f) (substitute var e x)
substitute _ _ e = e

whnf :: Expr -> Expr
whnf (Lam x (App f (Var x'))) | x == x' && not (x `freeIn` f) = whnf f
whnf (App f x) = fromMaybe (App f' x) $ whnf <$> apply f' x
  where f' = whnf f
whnf e = e

nf :: Expr -> Expr
nf (Lam x e) = whnf . Lam x . nf $ e
nf (App f x) = fromMaybe (App f' (nf x)) $ nf <$> apply f' x
  where f' = nf f
nf e = whnf e

apply :: Expr -> Expr -> Maybe Expr
apply (Lam x e) x' = Just $ substitute x x' e
apply (PrimFunc f) x = f x
apply _ _ = Nothing

toInt :: Expr -> Int
toInt x = toInt' . nf $ App (App x (PrimFunc (inc . nf))) (PrimInt 0)
  where
  inc (PrimInt x) = Just . PrimInt $ x + 1
  inc _ = Nothing
  toInt' (PrimInt x) = x
  toInt' x = error $ "Expression did not evaluate to an integer: " ++ show x

-- TODO clean this up
toIntMaybe :: Expr -> Maybe Int
toIntMaybe x = toIntMaybe' . nf $ App (App x (PrimFunc (inc . nf))) (PrimInt 0)
  where
  inc (PrimInt x) = Just . PrimInt $ x + 1
  inc _ = Nothing
  toIntMaybe' (PrimInt x) = Just x
  toIntMaybe' _ = Nothing

toIO :: Expr -> IO Expr
toIO e = toIO' $ nf e
  where
  toIO' (PrimIO io) = io
  toIO' (App io e) = do
    e' <- toIO io
    toIO (App e e')
  toIO' e = error $ "Expression did not evaluate to an IO: " ++ show e