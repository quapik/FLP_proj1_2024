--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
import System.Environment (getArgs)
import System.IO (openFile, hGetContents, hSetEncoding, IOMode (ReadMode))
import Text.Parsec
    ((<|>), anyChar, oneOf,try, alphaNum, space, newline, manyTill, char, digit, spaces, string, many1, sepEndBy, endBy, parse, many, noneOf)
import Text.Parsec.String (Parser)
import Control.Monad (replicateM_)
import Text.Parsec.Combinator(eof)
import GHC.IO.Encoding (utf8)



--Kontrola správně zadaných argumentů, pouze dvě možnosti, jinak chyba
argsChecker ::  [String] -> Bool
argsChecker [x,_] = x == "-2"
argsChecker [x,_,_] = x == "-1"
argsChecker _  = False

--Extraktuje nazvy souboru u argumentu do tuplu
fileExtract :: [String] -> (String, String)
fileExtract [_, file] = (file, "")
fileExtract [_, file1, file2] = (file1,file2)
fileExtract _ = ("", "")

data Tree = Leaf String | Node Int Float Tree Tree deriving Show
------------------------------------SOUBOR SE STROMEM

--Přečtení souboru se stromem a spuštění jeho parseru. Pokud uspěje, vrátí se načtený strom, jinak chyba
readTreeInputAndParse :: FilePath -> IO Tree --co tu ten IO?
readTreeInputAndParse inputFile =
  --Open file místo read kvůli tomu aby se vpohodě načítala diakritika a dalo se s ní pracovat
  openFile inputFile ReadMode >>=
  \fileHandle -> hSetEncoding fileHandle utf8 >>
  hGetContents fileHandle >>=
  \input -> case parse (nodeParser 0)  "" input of
    Left err -> error $  "Chyba pri nacitani vstupniho soboru se stromem: " ++ show err --TODO ERROR HANDLER
    Right loadedTree -> return loadedTree

--Vyzkouší všechny parsery co mohou být 
tryParsers :: Int -> Parser Tree
tryParsers pocet_mezer =  try (leafParser $ pocet_mezer + 2)  <|> try (leafParserEOF $ pocet_mezer + 2) <|> try (nodeParser $ pocet_mezer + 2)

--Zkontroluje odsazení, najde klíčové slovo Leaf:  a načte třídu pro daný Leaf
leafParser :: Int -> Parser Tree
leafParser pocet_mezer =
  replicateM_ pocet_mezer space >>
  string "Leaf: " >>
  fmap Leaf (manyTill anyChar (string "\n" <|> string "\r\n"))--kvuli linux/win newlinum

--Stejná funkce, ale pro pripad ze tam je eof -> krkolomné, ale nepodařilo se mi zkombinovat newline <|> eof v jedne funkci
leafParserEOF :: Int -> Parser Tree
leafParserEOF pocet_mezer =
  replicateM_ pocet_mezer space >>
  string "Leaf: " >>
  fmap Leaf (manyTill anyChar eof)

--Zkontroluje odsazení, najde klíčové slovo Node: , načte index příznaku a pak celou a desetinou část prahu (aby šlo na float)
nodeParser :: Int -> Parser Tree
nodeParser pocet_mezer = do
  odsazeni <- replicateM_ pocet_mezer space
  string "Node: "
  index_priznaku <- many1 digit
  string ", "
  prah_cela <- many1 digit
  char '.'
  prah_desetinna <- manyTill digit (string "\n" <|> string "\r\n") --kvuli linux/win newlinum
  l <- tryParsers pocet_mezer
  r <- tryParsers pocet_mezer
  return (Node (read index_priznaku) (read (prah_cela ++ "." ++ prah_desetinna)) l r)


--SKOPIROVANY, ODSTRAN!
printTree :: Tree -> String
printTree (Leaf label) = "Leaf: " ++ label ++ "\n"
printTree (Node index prah left right) =
  "Node: " ++ show index ++ ", " ++ show prah ++ "\n" ++
  printTree left ++
  printTree right

------------------------------------SOUBOR s FLOATAMA
--Funkce dostane načtený soubor rozdělený do listu podle řádků a vrací list listů floatů s jednotlivými body
fileStringToFloats :: [String] -> [[Float]]
fileStringToFloats [] = []
fileStringToFloats (x:xs)  = splitOnString x : fileStringToFloats xs

splitOnString  :: String -> [Float]
splitOnString  "" = []
splitOnString  xs = read (takeWhile (/= ',') xs) : splitOnString (drop 1 (dropWhile (/= ',') xs))

--Prochází pustupně jednotlivé řádky floatů (1 řádek -> nové dato pro kterou se určuje tříada)
prochazejData ::  Tree -> [[Float]] -> [String]
prochazejData _ [] = []
prochazejData tree (f:fs) = findInTree tree f : prochazejData tree fs

--Funkce dostane strom (nazačátku tam bude  první node) a jeden řádek s floaty pro vyhodnocení, dokud nenarazí na leaf tak prochází podstromy a porovnávaá
findInTree :: Tree -> [Float] -> String
findInTree (Leaf trida) _ = trida
findInTree (Node i value l r) floats
  | getValueForComparsion i floats <= value =  findInTree l floats
  | otherwise = findInTree r floats

--Získá správný float pro porovnávání s hodnotou nodu
getValueForComparsion :: Int -> [Float] -> Float
getValueForComparsion 0 (x:_) = x
getValueForComparsion index (_:xs) = getValueForComparsion (index-1) xs

printMSG :: String -> IO ()
printMSG = putStr

------------------------------------ DRUHY PODUKOL
data TrenovaciData = TrenovaciData{floats :: [Float], trida :: String} deriving Show

extractData :: [String] -> [TrenovaciData]
extractData [] = []
extractData (x:xs) = splitFloatsAndClass x : extractData xs

--Funkce vezme jeden řádek a ten převádí na TrenovaciData, převede [String] -> [Float] pro všechny krom posledního a s poslením uloží do struktury
splitFloatsAndClass :: String -> TrenovaciData
splitFloatsAndClass str = TrenovaciData (map read $ init $ splitOnStringToString str) (last $ splitOnStringToString str)

--Funkce co rozdělí string do pole stiringů podle čárky
splitOnStringToString  :: String -> [String]
splitOnStringToString  "" = []
splitOnStringToString  xs = takeWhile (/= ',') xs : splitOnStringToString (drop 1 (dropWhile (/= ',') xs))

printTrenovaciData:: TrenovaciData -> IO ()
printTrenovaciData trenovacidata = do
  print (floats trenovacidata)
  putStrLn (trida trenovacidata)

main :: IO()
main = do
    args <- getArgs
    if argsChecker args then  do
      if snd  (fileExtract args) == "" then do
       file2_content <- readFile (fst $ fileExtract args)
       let dataRecords = extractData (lines file2_content)
       mapM_ printTrenovaciData dataRecords
      else do
       file1_content <- readTreeInputAndParse (fst $ fileExtract args)
       --putStr (printTree file1_content)
       file2_content <- readFile (snd $ fileExtract args)
       --print res
       --Print tříd pro 
       putStr .  unlines $ prochazejData file1_content $ fileStringToFloats (lines file2_content)

    else do
        putStr "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    return ()
