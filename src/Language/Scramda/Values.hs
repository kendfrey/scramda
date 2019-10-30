{-# LANGUAGE OverloadedStrings #-}

module Language.Scramda.Values
  ( id
  , fix
  , flip
  , nonnormal
  ) where

import Prelude hiding (id, flip)
import Language.Scramda
import Language.Scramda.DSL

id :: Expr
id = x !> x

fix :: Expr
fix = f !> (x !> f ! (x ! x)) ! (x !> f ! (x ! x))

flip :: Expr
flip = f !> x !> y !> f ! y ! x

nonnormal :: Expr -> Expr
nonnormal x = flip ! (flip ! x)