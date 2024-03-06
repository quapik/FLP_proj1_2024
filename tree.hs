--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
import System.Environment (getArgs)
import System.IO (openFile, hGetContents, hSetEncoding, hClose, IOMode (ReadMode))
import Text.Parsec
    ((<|>), anyChar, oneOf,try, alphaNum, space, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf)
import Text.Parsec.String (Parser)
import Control.Monad (replicateM)
import Text.Parsec.Combinator(eof)
import GHC.IO.Encoding (getLocaleEncoding, utf8)
import Data.Array (Ix(index))
import Text.XHtml (treeColors)


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

data Tree = Leaf String | Node Int Double Tree Tree deriving Show
------------------------------------SOUBOR SE STROMEM
--Přečtení souboru se stromem a spuštění jeho parseru. Pokud uspěje, vrátí se načtený strom, jinak chyba
readTreeInputAndParse :: FilePath -> IO Tree
readTreeInputAndParse inputFile =  do
  --Kvůli tomu aby se vpohodě načítala diakritika a dalo se s ní pracovat
  fileHandle <- openFile inputFile ReadMode
  hSetEncoding fileHandle utf8
  input <- hGetContents fileHandle
  -- input <- readFile inputFile -- tohle pokud neresis to UTF
  case parse (nodeParser 0)  "" input of
    Left err -> error $  "Chyba pri nacitani vstupniho soboru se stromem: " ++ show err --TODO ERROR HANDLER
    Right loadedTree -> return loadedTree

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

------------------------------------SOUBOR s FLOATAMA
--Funkce dostane načtený soubor rozdělený do listu podle řádků a vrací list listů floatů s jednotlivými body
fileStringToFloats :: [String] -> [[Float]]
fileStringToFloats [] = []
fileStringToFloats (x:xs)  = splitString x : fileStringToFloats xs

-- Funkce, co dostane string, oddělí první substring před čárkou, ten převede na float, zbatek zbaví té čárky a rekurzivně volá
splitString :: String -> [Float]
splitString "" = []
splitString str = 
    let (x, xs_w_splitter) = break (==',') str
        (_, xs) = span (==',') xs_w_splitter
     in read x : splitString xs

prochazejData ::  Tree -> [[Float]] -> [String]
prochazejData _ [] = []
prochazejData tree (f:fs) = findInTree tree f : prochazejData tree fs

findInTree :: Tree -> [Float] -> String
-- findInTree tree floats =
--  case getNodeIndex tree of
--         Just index -> "Index nalezen: " ++ show index
--         Nothing -> "Index nenalezen"
findInTree tree floats = "asda"

getNodeValue :: Tree -> Maybe Double
getNodeValue (Node _ nodeValue _ _) = Just nodeValue
getNodeValue(Leaf _) = Nothing

getNodeIndex :: Tree -> Maybe Int
getNodeIndex (Node index _ _ _) = Just index
getNodeIndex (Leaf _) =  Nothing

getLeafClass:: Tree -> String
getLeafClass (Leaf trida) = trida


main :: IO()
main = do
    args <- getArgs
    if argsChecker args then  do
       let (file1, file2) = fileExtract args
       file1_content <- readTreeInputAndParse file1
       putStr (printTree file1_content)

       file2_content <- readFile file2
       let res = fileStringToFloats (lines file2_content)
       print res

       let res2 = prochazejData file1_content res
       print res2
       
    else do
        putStr "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    return ()
