{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Execution (tests) where

import Data.IORef
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)
import Language.Scramda
import Language.Scramda.DSL

id' :: Expr
id' = x !> x

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
  ,
    testGroup "toIO"
    [
      testCase "simple" do
        x <- newIORef 0
        toIO $ PrimIO do
          writeIORef x 42
          return id'
        y <- readIORef x
        assertEqual "Should execute primitive IO"
          42
          y
    ,
      testCase "nf" do
        x <- newIORef 0
        toIO . doubleFlip $ PrimIO do
          writeIORef x 42
          return id'
        y <- readIORef x
        assertEqual "Should reduce to NF"
          42
          y
    ,
      testCase "parameterized" do
        x <- newIORef 0
        let save = PrimFunc \y -> Just $ PrimIO do
              writeIORef x $ toInt y
              return id'
        toIO $ App save three
        y <- readIORef x
        assertEqual "Should execute parameterized IO"
          3
          y
    ,
      testCase "unevaluated parameters" do
        x <- newIORef 0
        let save = PrimFunc \y -> do
              y' <- toIntMaybe y -- TODO make a way to write this boilerplate automatically
              return $ PrimIO do
                writeIORef x y'
                return id'
        toIO $ App (doubleFlip save) three
        y <- readIORef x
        assertEqual "Should work with unevaluated parameters"
          3
          y
    ,
      testCase "composed" do
        x <- newIORef 0
        let load = PrimIO $ return three
        let save = PrimFunc \y -> Just $ PrimIO do
              writeIORef x $ toInt y
              return id'
        toIO $ App load save
        y <- readIORef x
        assertEqual "Should execute composed IO"
          3
          y
    ]
  ]