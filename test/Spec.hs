import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)
import Scramda

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Eq"
    [ testCase "variable" $ do
        assertEqual "Variable equality failed"
          (Var "x")
          (Var "x")
    , testCase "lambda" $ do
        assertEqual "Lambda equality failed"
          (Lam "x" (Var "x"))
          (Lam "x" (Var "x"))
    , testCase "apply" $ do
        assertEqual "Apply equality failed"
          (App (Var "x") (Var "y"))
          (App (Var "x") (Var "y"))
    ]

  , testGroup "Show"
    [ testCase "variable" $ do
        assertEqual "Variable show failed"
          "x"
          (show $ Var "x")
    , testCase "lambda" $ do
        assertEqual "Lambda show failed"
          "x -> x"
          (show $ Lam "x" (Var "x"))
    , testCase "apply" $ do
        assertEqual "Apply show failed"
          "x y"
          (show $ App (Var "x") (Var "y"))
    , testCase "combined" $ do
        assertEqual "Combined show failed"
          "(x -> x) (y (z -> z z))"
          (show $ App (Lam "x" (Var "x")) (App (Var "y") (Lam "z" (App (Var "z") (Var "z")))))
    ]
  ]