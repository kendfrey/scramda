{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Execution (tests) where

import Data.IORef
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)
import Language.Scramda
import Language.Scramda.DSL
import Language.Scramda.Primitives
import Language.Scramda.Values as S

three :: Expr
three = f !> x !> f ! (f ! (f ! x))

tests :: [Test]
tests =
  [
    testGroup "toInt"
    [
      testCase "simple" do
        assertEqual "Should work with the simplest expression"
          (Just 3)
          (toIntMaybe three)
    ,
      testCase "nf" do
        assertEqual "Should reduce to NF"
          (Just 3)
          (toIntMaybe $ f !> x !> f ! (f ! (f ! nonnormal x)))
    ]
  ,
    testGroup "toIO"
    [
      testCase "simple" do
        x <- newIORef (0 :: Int)
        toIO . toExpr $ writeIORef x 42
        y <- readIORef x
        assertEqual "Should execute primitive IO"
          42
          y
    ,
      testCase "nf" do
        x <- newIORef (0 :: Int)
        toIO . nonnormal . toExpr $ writeIORef x 42
        y <- readIORef x
        assertEqual "Should reduce to NF"
          42
          y
    ,
      testCase "parameterized" do
        x <- newIORef (0 :: Int)
        let save = toExpr $ writeIORef x
        toIO $ App save three
        y <- readIORef x
        assertEqual "Should execute parameterized IO"
          3
          y
    ,
      testCase "unevaluated parameters" do
        x <- newIORef (0 :: Int)
        let save = toExpr $ writeIORef x
        toIO $ App (nonnormal save) three
        y <- readIORef x
        assertEqual "Should work with unevaluated parameters"
          3
          y
    ,
      testCase "composed" do
        x <- newIORef (0 :: Int)
        let load = PrimIO $ return three
        let save = toExpr $ writeIORef x
        toIO $ App load save
        y <- readIORef x
        assertEqual "Should execute composed IO"
          3
          y
    ]
  ]