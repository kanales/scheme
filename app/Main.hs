module Main where

import System.Environment
import Scheme.Eval
import Scheme

main :: IO ()
main = do
    args <- getArgs 
    mapM_ (print . eval . readExpr) args
