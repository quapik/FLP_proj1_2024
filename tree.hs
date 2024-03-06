--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import System.Environment (getArgs)
import System.IO (openFile, hGetContents,isEOF)
import Text.Parsec
    ((<|>), anyChar, oneOf, anyToken, try, alphaNum, space, option, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf)
import Text.Parsec.String (Parser)
import Control.Monad (replicateM)
import Text.Parsec.Combinator(eof)


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

--Přečtení souboru se stromem a spuštění jeho parseru. Pokud uspěje, vrátí se načtený strom, jinak chyba
readTreeInputAndParse :: FilePath -> IO Tree
readTreeInputAndParse inputFile =  do
  input <- readFile inputFile
  case parse (nodeParser 0)  "" input of
    Left err -> error $ "Chyba pri nacitani vstupniho soboru se stromem: " ++ show err --TODO ERROR HANDLER
    Right loadedTree -> return loadedTree

data Tree = Leaf String | Node Int Double Tree Tree deriving Show

--Vyzkouší všechny parsery co mohou být 
tryParsers :: Int -> Parser Tree
tryParsers pocet_mezer =  try (leafParser $ pocet_mezer + 2)  <|> try (leafParserEOF $ pocet_mezer + 2) <|> try (nodeParser $ pocet_mezer + 2)

--Zkontroluje odsazení, najde klíčové slovo Leaf:  a načte třídu pro daný Leaf
leafParser :: Int -> Parser Tree
leafParser pocet_mezer = do
  odsazeni <- replicateM pocet_mezer space
  string "Leaf: "
  trida <-  manyTill anyChar newline
  return (Leaf trida)

--Stejná funkce, ale pro pripad ze tam je eof -> krkolomné, ale nepodařilo se mi zkombinovat newline <|> eof v jedne funkci
leafParserEOF :: Int -> Parser Tree
leafParserEOF pocet_mezer = do
  odsazeni <- replicateM pocet_mezer space
  string "Leaf: "
  trida <-  manyTill anyChar eof
  return (Leaf trida)

--Zkontroluje odsazení, najde klíčové slovo Node: , načte index příznaku a pak celou a desetinou část prahu (aby šlo na float)
nodeParser :: Int -> Parser Tree
nodeParser pocet_mezer = do
  odsazeni <- replicateM pocet_mezer space
  string "Node: "
  index_priznaku <- many1 digit
  string ", "
  prah_cela <- many1 digit
  char '.'
  prah_desetinna <- manyTill digit newline 
  l <- tryParsers pocet_mezer
   --spaces
  --char '\n'
  r <- tryParsers pocet_mezer
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
       putStr (printTree content)
    else do
        putStr "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    
    return ()
