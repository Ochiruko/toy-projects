module Main where

import Text.ParserCombinators.Parsec hiding (spaces, hexDigit)
import System.Environment
import Control.Monad
import Data.Char (toLower)
import Numeric

main :: IO ()
main = do (a:_) <- getArgs
          print $ readExpr a

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" -- does not contain \' or \"

spaces :: Parser ()
spaces = skipMany1 space -- requires whitespace at beginning

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             deriving Show

parseString :: Parser LispVal
parseString = 
  do char '"'
     str <- many $ noneOf "\\\""
               <|> do char '\\'
                      c <- oneOf "ntr\\\""
                      return $ case c of
                        'n'  -> '\n'
                        't'  -> '\t'
                        'r'  -> '\r'
                        '\\' -> '\\'
                        '"'  -> '"'
     char '"'
     return . String $ str

-- The way parseAtom works is insanely cool to me, as you can only really
-- do this in Haskell. Because Parser is a Monad, you can do things with
-- the eventual output in e.g. a do statement:
--     (p :: Parser LispVal) >>= (f :: LispVal -> Parser b)
-- or better yet:
--     (p :: Parser LispVal) >>= return . (g :: LispVal -> b) $
-- which is effectively equivalent to
--     do x <- p
--        return $ g x
-- except it doesn't actually have anything to parse yet, which is fine,
-- because there is no extract function; in fact, until you run
--     parse p "input label" input
-- or an abstracted version of parse, your parser can't really *do*
-- anything.
-- NOTE: p <|> q tries p then q.
-- (atom | bool) ::= ([a-z] | [\symbol]) ([a-z] | [\symbol] | [0-9])*
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol -- Parser LispVal |> LispVal
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first : rest -- LispVal : LispVal
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

-- <number> ::= <decimal>
--            | <binary>
--            | <octal>
--            | <hexadecimal>
-- 
-- <decimal> ::= <digit>+
-- <binary> ::= #b<binary-digit>+
-- <octal> ::= #o<octal-digit>+
-- <hexadecimal> ::= #x<hex-digit>+
-- 
-- <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- <binary-digit> ::= 0 | 1
-- <octal-digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
-- <hex-digit> ::= <digit> | a | b | c | d | e | f

extractReadS :: [(a, b)] -> a
extractReadS = fst . head

binaryDigit :: Parser Char
binaryDigit = char '0' <|> char '1'

octalDigit :: Parser Char
octalDigit = choice . map char . map head . map show $ [0..7]

hexDigit :: Parser Char
hexDigit = digit <|> liftM toLower (oneOf "abcdefABCDEF")

parseBinary :: Parser LispVal
parseBinary = do n <- many1 binaryDigit
                 return . Number . extractReadS . readBin $ n

parseOctal :: Parser LispVal
parseOctal = do n <- many1 octalDigit
                return . Number . extractReadS . readOct $ n

parseHex :: Parser LispVal
parseHex = do n <- many1 hexDigit
              return . Number . extractReadS . readHex $ n

parseDec :: Parser LispVal
parseDec = do n <- many1 digit
              return . Number . extractReadS . readDec $ n

parseNumber :: Parser LispVal
parseNumber = 
  do char '#'
     c <- oneOf "box"
     case c of
       'b' -> parseBinary
       'o' -> parseOctal
       'x' -> parseHex
       _   -> fail "must have been a glitch in the system..."
  <|> parseDec

parseChar :: Parser LispVal
parseChar = 
  do char '#'
     char '\\'
     s <- liftM (:[]) symbol <|> liftM (:[]) (char ' ') <|> many1 letter
     return $ case length s > 1 of
       False -> Char . head $ s
       True  -> Char . matchCharCode $ s

matchCharCode :: String -> Char
matchCharCode s = case s of
  "space" -> ' '
  "newline" -> '\n'

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _  -> "Found value"
