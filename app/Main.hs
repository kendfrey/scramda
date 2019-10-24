module Main where

import Language.Scramda
import Language.Scramda.DSL

main :: IO ()
main = putStrLn . show $ x !> x