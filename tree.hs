--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
import System.Environment (getArgs)
import System.IO (openFile, hGetContents)
import Control.Monad
import Text.Parsec
    ((<|>), anyChar, option, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf, Parsec )
import Text.Parsec.String (Parser)
import Distribution.Simple.Command (ShowOrParseArgs(ParseArgs))


--Kontrola správně zadaných argumentů
argsChecker ::  [String] -> Bool
argsChecker [x,_] = x == "-2" 
argsChecker [x,_,_] = x == "-1"
argsChecker _  = False

fileExtract :: [String] -> (String, String)
fileExtract [_, file] = (file, "")
fileExtract [_, file1, file2] = (file1,file2)
fileExtract [_] = ("", "")

readTreeFile :: FilePath -> Tree
readTreeFile treeFile =  nodeParser (readFile treeFile)

data Tree = Leaf String | Node Int Double Tree Tree deriving Show

leafParser :: Parser Tree
leafParser = do
  spaces
  string "Leaf: "
  trida <-  manyTill anyChar newline 
  char '\n'
  return (Leaf trida)

nodeParser :: Parser Tree
nodeParser = do
  spaces --jsou tu ty spaces potreba?
  string "Node: "
  index_priznaku <- many1 digit
  char ','
  prah_cela <- many1 digit
  char '.'
  prah_desetinna <- many1 digit
  l <- nodeParser
  spaces
  char '\n'
  r <- nodeParser
  return (Node (read index_priznaku) (read (prah_cela ++ "." ++ prah_desetinna)) l r) --tohle asi upravit, melo by to jit i jinak
  <|> leafParser

--SKOPIROVANY, ODSTRAN!
printTree :: Tree -> String
printTree (Leaf label) = "Leaf: " ++ label ++ "\n"
printTree (Node id threshold left right) =
  "Node: " ++ show id ++ ", " ++ show threshold ++ "\n" ++
  printTree left ++
  printTree right


main :: IO()
main = do
    args <- getArgs
    if (argsChecker args) then  do
       let (file1, file2) = (fileExtract args)
       --if file2 == "" then putStr ("jedna") else putStr "dva" 
       content <- readTreeFile file1
       putStrLn (printTree content)
       putStrLn ("ST")
    else do
        putStr "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    
    return ()
