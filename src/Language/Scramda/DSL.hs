{-# LANGUAGE FlexibleInstances #-}

module Language.Scramda.DSL
  ( (!>)
  , (!)
  , a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
  ) where

import Language.Scramda

class IsExpr a where
  expr :: a -> Expr

instance IsExpr Expr where
  expr = id

instance IsExpr String where
  expr = Var

infixr 8 !>
(!>) :: IsExpr a => String -> a -> Expr
x !> e = Lam x (expr e)

infixl 9 !
(!) :: (IsExpr a, IsExpr b) => a -> b -> Expr
f ! x = App (expr f) (expr x)

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"
f = "f"
g = "g"
h = "h"
i = "i"
j = "j"
k = "k"
l = "l"
m = "m"
n = "n"
o = "o"
p = "p"
q = "q"
r = "r"
s = "s"
t = "t"
u = "u"
v = "v"
w = "w"
x = "x"
y = "y"
z = "z"