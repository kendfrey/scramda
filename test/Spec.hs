import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Scramda

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Eq"
    [ testCase "variable" $ assertEqual "Variable equality failed" (Var "x") (Var "x")
    , testCase "lambda" $ assertEqual "Lambda equality failed" (Lam "x" (Var "x")) (Lam "x" (Var "x"))
    , testCase "apply" $ assertEqual "Apply equality failed" (App (Var "x") (Var "y")) (App (Var "x") (Var "y"))
    ]
  , testGroup "Show"
    [ testCase "variable" $ assertEqual "Variable show failed" "Var \"x\"" (show $ Var "x")
    , testCase "lambda" $ assertEqual "Lambda show failed" "Lam \"x\" (Var \"x\")" (show $ Lam "x" (Var "x"))
    , testCase "apply" $ assertEqual "Apply show failed" "App (Var \"x\") (Var \"y\")" (show $ (App (Var "x") (Var "y")))
    ]
  ]