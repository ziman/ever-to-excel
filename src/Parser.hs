module Parser where

import Data.Char
import Text.Parsec
import Data.Text (Text)

type Parser = Parsec Text ()

data Atom
  = Symbol String
  | String String
  | Number Int
  deriving (Eq, Ord, Show)

parseAtom :: Parser Atom
parseAtom =
  parseNumber
  <|> parseString
  <|> parseSymbol

parseNumber :: Parser Atom
parseNumber = do
  digits <- many1 digit
  pure $ Number (read digits)

parseString :: Parser Atom
parseString = do
  _ <- string "\""
  content <- many (satisfy (/= '"'))
  _ <- string "\""
  pure $ String content

parseSymbol :: Parser Atom
parseSymbol = do
  stuff <- many1 (satisfy isSymbolC)
  pure $ Symbol stuff
 where
  isSymbolC c = not (isSpace c) && (c `notElem` "\"()[]{}.")
