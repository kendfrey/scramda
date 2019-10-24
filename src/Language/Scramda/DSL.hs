{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Language.Scramda.DSL
  ( (!>)
  , (!)
  , a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
  ) where

import Data.String
import Language.Scramda

instance IsString Expr where
  fromString = Var

infixr 8 !>
(!>) :: String -> Expr -> Expr
(!>) = Lam

infixl 9 !
(!) :: Expr -> Expr -> Expr
(!) = App

a :: IsString a => a
a = "a"
b :: IsString a => a
b = "b"
c :: IsString a => a
c = "c"
d :: IsString a => a
d = "d"
e :: IsString a => a
e = "e"
f :: IsString a => a
f = "f"
g :: IsString a => a
g = "g"
h :: IsString a => a
h = "h"
i :: IsString a => a
i = "i"
j :: IsString a => a
j = "j"
k :: IsString a => a
k = "k"
l :: IsString a => a
l = "l"
m :: IsString a => a
m = "m"
n :: IsString a => a
n = "n"
o :: IsString a => a
o = "o"
p :: IsString a => a
p = "p"
q :: IsString a => a
q = "q"
r :: IsString a => a
r = "r"
s :: IsString a => a
s = "s"
t :: IsString a => a
t = "t"
u :: IsString a => a
u = "u"
v :: IsString a => a
v = "v"
w :: IsString a => a
w = "w"
x :: IsString a => a
x = "x"
y :: IsString a => a
y = "y"
z :: IsString a => a
z = "z"