--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import System.Environment (getArgs)
import System.IO (openFile, hGetContents)
import Control.Monad
import Text.Parsec
    ((<|>), anyChar, try, option, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf)
import Text.Parsec.String (Parser)


--Kontrola správně zadaných argumentů, pouze dvě možnosti, jinak chyba
argsChecker ::  [String] -> Bool
argsChecker [x,_] = x == "-2" 
argsChecker [x,_,_] = x == "-1"
argsChecker _  = False

--Extraktuje nazvy souboru u argumentu do tuplu
fileExtract :: [String] -> (String, String)
fileExtract [_, file] = (file, "")
fileExtract [_, file1, file2] = (file1,file2)
fileExtract [_] = ("", "")

readTreeInputAndParse :: FilePath -> IO Tree
readTreeInputAndParse inputFile =  do
  input <- readFile inputFile
  case parse startParse  "" input of
    Left err -> error $ "Error parsing tree file: " ++ show err --TODO ERROR HANDLER
    Right loadedTree -> return loadedTree

startParse :: Text.Parsec.String.Parser Tree
startParse =  nodeParser <|> leafParser 

data Tree = Leaf String | Node Int Double Tree Tree deriving Show

leafParser :: Text.Parsec.String.Parser Tree
leafParser = do
  spaces
  string "Leaf: "
  trida <-  manyTill anyChar newline 
  return (Leaf trida)

nodeParser :: Text.Parsec.String.Parser Tree
nodeParser = do
  spaces --kvuli odsazeni
  string "Node: "
  index_priznaku <- many1 digit
  string ", "
  prah_cela <- many1 digit
  char '.'
  prah_desetinna <- manyTill digit newline 
  l <- try leafParser  <|> try nodeParser
   --spaces
  --char '\n'
  r <- try leafParser  <|> try nodeParser
  return (Node (read index_priznaku) (read (prah_cela ++ "." ++ prah_desetinna)) l r) 


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
    if argsChecker args then  do
       let (file1, file2) = fileExtract args
       --if file2 == "" then putStr ("jedna") else putStr "dva" 
       content <- readTreeInputAndParse file1
       putStrLn (printTree content)
       putStrLn ("ST")
    else do
        putStr "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    
    return ()
