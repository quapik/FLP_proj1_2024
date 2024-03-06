import System.Environment (getArgs)
import System.IO (openFile, hGetContents, hSetEncoding, hClose, IOMode (ReadMode))
import Text.Parsec
    ((<|>), anyChar, oneOf,try, alphaNum, space, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf)
import Text.Parsec.String (Parser)
import Control.Monad (replicateM)
import Text.Parsec.Combinator(eof)
import GHC.IO.Encoding (getLocaleEncoding, utf8)
import Data.List (lines)

a :: [String] -> [[Float]]
a [] = []
a (x:xs)  = splitString x : a xs

-- toFloat:: String -> [Float]
-- toFloat [] = []
-- --toFloat x = read (takeWhile (/=',') x) : toFloat (dropWhile (/=',')  x)
-- toFloat x = 1.0 : toFloat (dropWhile (==',')  x)

-- pp :: Parser [Float]
-- pp  = do
--   prah_cela <- many1 digit
--   char '.'
--   prah_desetinna <- manyTill digit newline 
--   x <- pp
--   return ( (read (prah_cela ++ "." ++ prah_desetinna)) : x)

splitString :: String -> [Float]
splitString "" = []
splitString str = 
    let (x, xs_w_splitter) = break (==',') str
        (_, xs) = span (==',') xs_w_splitter
     in read x : splitString xs

main :: IO()
main = do
  input <- readFile "input2"
  let res = a (lines input)
  print res
  return ()
