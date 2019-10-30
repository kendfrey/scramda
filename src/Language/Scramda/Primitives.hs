module Language.Scramda.Primitives
  ( fromExpr
  , toExpr
  ) where

import Language.Scramda
import Language.Scramda.Values as S

class FromExpr a where
  fromExpr :: Expr -> Maybe a

instance FromExpr Int where
  fromExpr = toIntMaybe

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr () where
  toExpr _ = S.id

instance ToExpr a => ToExpr (IO a) where
  toExpr = PrimIO . fmap toExpr

instance (FromExpr a, ToExpr b) => ToExpr (a -> b) where
  toExpr f = PrimFunc $ fmap (toExpr . f) . fromExpr