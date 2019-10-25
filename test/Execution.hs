{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Execution (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)
import Language.Scramda
import Language.Scramda.DSL

three :: Expr
three = f !> x !> f ! (f ! (f ! x))

flip' :: Expr
flip' = f !> x !> y !> f ! y ! x

doubleFlip :: Expr -> Expr
doubleFlip x = flip' ! (flip' ! x)

tests :: [Test]
tests =
  [
    testGroup "toInt"
    [
      testCase "simple" do
        assertEqual "Should work with the simplest expression"
          3
          (toInt three)
    ,
      testCase "nf" do
        assertEqual "Should reduce to NF"
          3
          (toInt $ f !> x !> f ! (f ! (f ! doubleFlip x)))
    ]
  ]