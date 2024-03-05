--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import System.Environment (getArgs)
import System.IO (openFile, hGetContents)
import Control.Monad
import Text.Parsec
    ((<|>), anyChar, try, option, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf, Parsec)

mainParser :: Parsec String () String
mainParser = do
  result <- try (string "new" >> newParser) <|> try (string "call" >> callParser) <|> otherParser
  return result
  
newParser :: Parsec String () String
newParser = do
  string "content"
  return "New content"

callParser :: Parsec String () String
callParser = do
  string "me"
  return "Calling me"

otherParser :: Parsec String () String
otherParser = do
  content <- many anyChar
  return content

main :: IO ()
main = do
  let input = "callme"
  case parse mainParser "" input of
    Left err -> print err
    Right result -> print result