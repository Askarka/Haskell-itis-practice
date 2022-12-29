module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Control.Monad
import System.Environment
import Data.Char

type Parser = Parsec Void String

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\""

csvRes :: [[String]]
csvRes =
  [ [ "col1",  "col2",  "col3" ]
  , [ "r2 c1", "r2 c2", "r2 c3" ]
  , [ "r3,c1", "r3,c2", "r3,c3" ]
  ]


tks = [' ', '!', '-', '?', '$', '#', '%', '&', '*']

inQuotes = ['\\', '\"', '\'']


inQParser = char '\\' *> oneOf inQuotes 



fieldParser :: Parser String
fieldParser = (char '\"' *> some (alphaNumChar <|> inQParser <|> char ',' <|> oneOf tks) <* char '\"') <|> some (alphaNumChar <|> oneOf tks)

fieldCommaParser :: Parser String
fieldCommaParser = do
  field <- fieldParser
  _ <- single ','
  pure field

fieldsParser :: Parser [String]
fieldsParser = many $ try fieldCommaParser

colParser :: Parser [String]
colParser = do
  fields <- optional $ try fieldsParser
  f <- fieldParser
  case fields of
    Just fs -> pure $ fs ++ [f]
    Nothing -> pure [f]

colEolParser :: Parser [String]
colEolParser = do
  col <- colParser
  _ <- single '\n'
  pure col

colsParser :: Parser [[String]]
colsParser = many $ try colEolParser

csvParser :: Parser [[String]]
csvParser = do
  cols <- optional $ try colsParser
  c <- colParser
  case cols of
    Just cs -> pure $ cs ++ [c]
    Nothing -> pure [c]

main :: IO ()
main = pure ()
