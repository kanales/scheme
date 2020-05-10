module Scheme where

import Scheme.Parser
import Scheme.Tree

readExpr :: String -> LispVal
readExpr input = case parse input of
                 Left err -> String $ "No match:" ++ show err
                 Right val -> val

