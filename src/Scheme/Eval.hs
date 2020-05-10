module Scheme.Eval where

import Scheme.Tree

eval :: LispVal -> LispVal
eval (String s) = String s
eval (Number n) = Number n
eval (Bool b) = Bool b
eval (List [Atom "quote", val]) = val
eval _ = undefined -- TODO
