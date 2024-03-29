{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Expression (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)
import Language.Scramda
import Language.Scramda.DSL

tests :: [Test]
tests =
  [
    testGroup "freeIn"
    [
      testCase "variable" do
        assertEqual "Free variable not found"
          True
          (x `freeIn` Var x)
        assertEqual "Free variable mistakenly found"
          False
          (x `freeIn` Var y)
    ,
      testCase "lambda" do
        assertEqual "Free variable not found"
          True
          (x `freeIn` y !> x)
        assertEqual "Free variable mistakenly found"
          False
          (x `freeIn` x !> x)
    ,
      testCase "apply" do
        assertEqual "Free variable not found"
          True
          (x `freeIn` x ! y)
        assertEqual "Free variable mistakenly found"
          False
          (x `freeIn` y ! z)
    ,
      testCase "combined" do
        assertEqual "Free variable not found"
          True
          (y `freeIn` (x !> x) ! (y ! (z !> z ! z)))
        assertEqual "Free variable mistakenly found"
          False
          (x `freeIn` (x !> x) ! (y ! (z !> z ! z)))
    ]
  ,
    testGroup "substitute"
    [
      testCase "variable" do
        assertEqual "Variable substitution failed"
          (Var y)
          (substitute x (Var y) (Var x))
    ,
      testCase "lambda" do
        assertEqual "Lambda substitution failed"
          (z !> y ! z)
          (substitute x (Var y) (z !> x ! z))
        assertEqual "Capture avoiding substitution failed"
          ("y$" !> y ! "y$")
          (substitute x (Var y) (y !> x ! y))
    ,
      testCase "apply" do
        assertEqual "Apply substitution failed"
          (y ! z)
          (substitute x (Var y) (x ! z))
    ]
  ,
    testGroup "whnf"
    [
      testCase "variable" do
        assertEqual "Variables should be WHNF"
          (Var x)
          (whnf $ Var x)
    ,
      testCase "lambda" do
        assertEqual "Lambdas should be WHNF"
          (x !> x)
          (whnf $ x !> x)
    ,
      testCase "apply" do
        assertEqual "Apply should not be WHNF"
          (Var y)
          (whnf $ (x !> x) ! y)
    ,
      testCase "whnf" do
        assertEqual "Should not reduce to NF"
          (x !> x ! ((y !> y) ! x))
          (whnf $ x !> x ! ((y !> y) ! x))
    ,
      testCase "eta-whnf" do
        assertEqual "Should reduce to eta-WHNF"
          (Var y)
          (whnf $ x !> y ! x)
    ,
      testCase "normal-order reduction" do
        assertEqual "Should use normal-order reduction"
          (Var y)
          (whnf $ (x !> y) ! ((x !> x ! x) ! (x !> x ! x)))
    ]
  ,
    testGroup "nf"
    [
      testCase "nf" do
        assertEqual "Should reduce to NF"
          (x !> x ! x)
          (nf $ x !> x ! ((y !> y) ! x))
    ,
      testCase "normal-order reduction" do
        assertEqual "Should use normal-order reduction"
          (Var y)
          (nf $ (x !> y) ! ((x !> x ! x) ! (x !> x ! x)))
    ]
  ,
    testGroup "Eq"
    [
      testCase "variable" do
        assertEqual "Variable equality failed"
          (Var "x")
          (Var "x")
    ,
      testCase "lambda" do
        assertEqual "Lambda equality failed"
          (Lam "x" (Var "x"))
          (Lam "x" (Var "x"))
    ,
      testCase "apply" do
        assertEqual "Apply equality failed"
          (App (Var "x") (Var "y"))
          (App (Var "x") (Var "y"))
    ]
  ,
    testGroup "Show"
    [
      testCase "variable" do
        assertEqual "Variable show failed"
          "x"
          (show $ Var "x")
    ,
      testCase "lambda" do
        assertEqual "Lambda show failed"
          "x -> x"
          (show $ x !> x)
    ,
      testCase "apply" do
        assertEqual "Apply show failed"
          "x y"
          (show $ x ! y)
    ,
      testCase "combined" do
        assertEqual "Combined show failed"
          "(x -> x) (y (z -> z z))"
          (show $ (x !> x) ! (y ! (z !> z ! z)))
    ]
  ]