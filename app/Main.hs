module Main where

import Language.Scramda

main :: IO ()
main = putStrLn . show $ Lam "x" (Var "x")