module Scheme.Tree (LispVal(..)) where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal]  LispVal
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool
             | Vector [LispVal]
             deriving (Eq)

shList :: [LispVal] -> String 
shList = unwords . fmap show

instance Show LispVal where
    show (String s) = "\"" ++ s ++ "\""
    show (Atom a) = a
    show (Number n) = show n
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List cts) = "(" ++ shList cts ++ ")"
    show (DottedList heads tail) = 
        "(" ++ shList heads ++ " . " ++ show tail ++ ")"
    show (Float f) = show f
    show (Character c) = return c
    show (Vector v) = "#(" ++ shList v ++ ")"

