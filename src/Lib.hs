module Lib where

import Text.ParserCombinators.Parsec hiding (spaces, parse)
import qualified Text.ParserCombinators.Parsec as P

import Numeric
import Control.Monad
import System.Environment

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal]  LispVal
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool
             | Vector [LispVal]
             deriving (Show, Eq)

parseChar :: Parser LispVal
parseChar = try (string "#\\") >>
    Character <$> (try characterName <|> character)
    where 
        characterName = (string "newline" >> return '\n') 
                        <|> (string "space" >> return ' ')
        character = anyChar <* notFollowedBy alphaNum

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf "\"\\"
    char '"'
    return (String x)
        where
        escapedChar = do
            char '\\' -- escaped chars begin witha backslash
            x <- oneOf "\\\"nrt" -- either backslash or quote
            return $ case x of
                       'n' -> '\n'
                       'r' -> '\r'
                       't' -> '\t'
                       x -> x
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first : rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    ptrue <|> pfalse
        where
            ptrue = char 't' >> return (Bool True)
            pfalse = char 'f' >> return (Bool False)

parseFloat :: Parser LispVal
parseFloat = Float <$> do
    int <- many1 digit
    char '.'
    dec <- many1 digit
    return $ readFloat' (int ++ '.' : dec)
    where
        readFloat' = fst . head . readFloat

parseNumber :: Parser LispVal
parseNumber = Number <$>
    (parseDecimal <|> parseHex <|> parseOct <|> parseBin)
        where
            parseDecimal = do
                try (optional $ string "#d")
                read <$> many1 digit
            parseHex = do
                try (string "#x") 
                readHex' <$> many1 hexDigit 
            parseOct = do
                try (string "#o")
                readOct' <$> many1 octDigit
            parseBin = do
                try (string "#b")
                ds <- many1 (oneOf "01")
                return (asBin ds)


            readHex' = fst . head . readHex
            readOct' = fst . head . readOct
            asBin :: String -> Integer
            asBin = foldl ff 0
                where 
                    ff a '0' = 2 * a
                    ff a '1' = 2 * a + 1

parseSeq :: Parser [LispVal]
parseSeq = sepBy parseExpr spaces
                
parseList :: Parser LispVal
parseList = List <$> parseSeq

parseDottedList :: Parser LispVal
parseDottedList = DottedList
    <$> endBy parseExpr spaces
    <*> (char '.' >> spaces >> parseExpr)
    
parseQuoted :: Parser LispVal
parseQuoted = List <$> do
    char '\''
    x <- parseExpr
    return [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = List <$> do
    char '`'
    x <- parseExpr 
    return [Atom "quasiquote", x]


parseUnquoted :: Parser LispVal
parseUnquoted = List <$> do
    char ','
    x <- parseExpr 
    return [Atom "unquoted", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = List <$> do
    char ','
    x <- parseExpr 
    return [Atom "unquote-splicing", x]


parseVector :: Parser LispVal
parseVector =  try (string "#(") *> (Vector <$> parseSeq) <* char ')'

                
parseExpr :: Parser LispVal
parseExpr = choice 
    [ parseAtom 
    , parseString 
    , try parseFloat
    , parseNumber 
    , parseChar 
    , parseVector
    , parseBool
    , parseQuoted
    , parseQuasiQuoted 
    , parseUnquoted 
    , parseUnquoteSplicing
    , char '(' *> (try parseList <|> parseDottedList) <* char ')'
    ]

readExpr :: String -> String
readExpr input = case parse input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val 

parse :: String -> Either ParseError LispVal
parse = P.parse parseExpr "lisp"


