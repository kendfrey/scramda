module Main where

import Scramda

main :: IO ()
main = putStrLn . show $ Lam "x" (Var "x")