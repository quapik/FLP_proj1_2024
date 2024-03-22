--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
import System.Environment (getArgs)
--import System.IO (openFile, hGetContents, hSetEncoding, IOMode (ReadMode))
import Text.Parsec ((<|>), anyChar, try, space, manyTill, char, digit, string, many1, parse)
import Text.Parsec.String (Parser)
import Control.Monad (replicateM_)
import Text.Parsec.Combinator(eof)
--import GHC.IO.Encoding (utf8)
import Data.List (group, sort, partition,transpose)
import System.Exit (exitFailure)

--Pužívané datové typy
data TrenovaciData = TrenovaciData{floats :: [Float], trida :: String} deriving Show
data Tree = Leaf String | Node Int Float Tree Tree deriving Show

-- empty :: Tree
-- empty = Leaf "Empty"

------------------------------------ARGUMENTY
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

roundFloat :: Float -> Float
roundFloat x = fromIntegral (truncate $ x * 10) / 10

handleError :: String -> IO ()
handleError error_msg = putStrLn ("Nekde se vystkytla chyba!\n" ++ error_msg) >> exitFailure

------------------------------------NAČÍTÁNÍ PRVNÍHO SOUBORU A JEHO PARSOVÁNÍ
readTreeInputAndParse :: FilePath -> IO Tree
readTreeInputAndParse inputFile =
  readFile inputFile >>=
  \input -> case parse (nodeParser 0)  "" input of
    Left err -> error $  "Chyba pri nacitani vstupniho soboru se stromem: " ++ show err --TODO ERROR HANDLER
    Right loadedTree -> return loadedTree

--Vyzkouší všechny parsery co mohou být 
tryParsers :: Int -> Parser Tree
tryParsers pocet_mezer =  try (leafParser $ pocet_mezer + 2)  <|> try (leafParserEOF $ pocet_mezer + 2) <|> try (nodeParser $ pocet_mezer + 2)

--Zkontroluje odsazení, najde klíčové slovo Leaf:  a načte třídu pro daný Leaf
leafParser :: Int -> Parser Tree
leafParser pocet_mezer =
  replicateM_ pocet_mezer space >> string "Leaf: " >> fmap Leaf (manyTill anyChar (string "\n" <|> string "\r\n"))--kvuli linux/win newlinum

--Stejná funkce, ale pro pripad ze tam je eof -> krkolomné, ale nepodařilo se mi zkombinovat newline <|> eof v jedne funkci
leafParserEOF :: Int -> Parser Tree
leafParserEOF pocet_mezer =
  replicateM_ pocet_mezer space >> string "Leaf: " >>  fmap Leaf (manyTill anyChar eof)

--Zkontroluje odsazení, najde klíčové slovo Node: , načte index příznaku a pak celou a desetinou část prahu (aby šlo na float)
nodeParser :: Int -> Parser Tree
nodeParser pocet_mezer =
  replicateM_ pocet_mezer space >>  string "Node: " >> many1 digit >>= \index_priznaku ->
  string ", " >> many1 digit >>=   \prah_cela ->
  char '.' >> manyTill digit (string "\n" <|> string "\r\n") >>= \prah_desetinna ->  --kvuli linux/win newlinum
  tryParsers pocet_mezer >>= \l  ->
  tryParsers pocet_mezer >>= \r ->  return (Node (read index_priznaku) (read (prah_cela ++ "." ++ prah_desetinna)) l r)

--Funkce co vytiskne zadaný strom na out
printTree :: Tree -> Int -> IO ()
printTree (Leaf trida) odsazeni = putStrLn (replicate odsazeni ' ' ++ "Leaf: " ++ trida)
printTree (Node i prah l r) odsazeni = putStrLn (replicate odsazeni ' ' ++ "Node: " ++ show i ++ ", " ++ show  prah) >> printTree l (odsazeni + 2) >> printTree r (odsazeni + 2)

------------------------------------SOUBOR s FLOATAMA
--Funkce dostane načtený soubor rozdělený do listu podle řádků a vrací list listů floatů s jednotlivými body
fileStringToFloats :: [String] -> [[Float]]
fileStringToFloats = map splitOnString

--Seznam floatů v stringu parsne a vrátí z toho seznam floatů
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

------------------------------------ DRUHY PODUKOL

--Funkce vezme jeden řádek a ten převádí na TrenovaciData, převede [String] -> [Float] pro všechny krom posledního a s poslendím uloží do struktury
splitFloatsAndClass :: String -> TrenovaciData
splitFloatsAndClass radek = TrenovaciData (map read $ init $ splitOnStringToString radek) (last $ splitOnStringToString radek)

--Funkce co rozdělí string do pole stiringů podle čárky
splitOnStringToString  :: String -> [String]
splitOnStringToString  "" = []
splitOnStringToString  xs = takeWhile (/= ',') xs : splitOnStringToString (drop 1 (dropWhile (/= ',') xs))

--Funkce, co dostane trénovací data a postupně rekurzivně volá rozdělovací funkce a tvoří strom
--Pokud jeden prvek, automaticky leaf, pokud 2 a jsou stejné, tak už není co dělit a je to automaticky ta daná třída
vytvarejStrom :: [TrenovaciData] -> Tree
vytvarejStrom trenovaciDataList
 | length trenovaciDataList == 1 = Leaf (trida (head trenovaciDataList))
 | length trenovaciDataList == 2, trida (head trenovaciDataList) == trida (last trenovaciDataList) = Leaf (trida (head trenovaciDataList))
 | otherwise = Node nejmensi_index (roundFloat prah) (vytvarejStrom l) (vytvarejStrom r)
     where intervals =  createIntervals trenovaciDataList
           nejmensi_index = fst (minimizeIntervals trenovaciDataList intervals)
           prah  = intervals !! nejmensi_index !! snd (minimizeIntervals trenovaciDataList intervals)
           l = fst (splitTrenovaciData trenovaciDataList  prah nejmensi_index)
           r = snd (splitTrenovaciData trenovaciDataList  prah nejmensi_index)

--Vlastní funkce, co vrací index prvku v poli (vždy tam ten prvek bude, tudíž nemusí být Maybe a je to jednodušší na použití)
findElemIndex :: Float -> [Float]  -> Int -> Int
findElemIndex elem (x:xs) delka = if x == elem then delka - length xs -1 else findElemIndex elem xs delka

--Funkce vezme pro jednotlivé sloupce a vypočítá možné hodnoty pro tvoření splitů
createIntervals::  [TrenovaciData] -> [[Float]]
createIntervals trenovaciDataList =  map ((\l -> [(x1 + x2) / 2 | (x1, x2) <- zip l (tail l)]) . sort) (transpose (map floats trenovaciDataList))

--Funkce vezme data, intervaly a vyplivne pouze to, kde se nachází nejmenší Gsplit hodnota co se použije jako práh (index_intervalu, index_v_intervalu)
minimizeIntervals :: [TrenovaciData] -> [[Float]] -> (Int,Int)
minimizeIntervals trenovaciDataList intervals = (index_sloupce, index_v_intervalu)
  where nejmensi_splity = map fst (zipWith (findMinInInterval trenovaciDataList) intervals [0..])
        nejmensi_indexy = map snd (zipWith (findMinInInterval trenovaciDataList) intervals [0..])
        index_sloupce = findElemIndex (minimum nejmensi_splity) nejmensi_splity (length nejmensi_splity)
        index_v_intervalu = nejmensi_indexy !! index_sloupce

--Funkce volá zkoušení splitů a co najde minimum v intervalu a to, na jaké se nachází pozici 
findMinInInterval :: [TrenovaciData] -> [Float] -> Int -> (Float,Int)
findMinInInterval trenovaciDataList interval index = (minimum [checkGiniSplit trenovaciDataList prah index | prah <- interval] , findElemIndex min [checkGiniSplit trenovaciDataList prah index | prah <- interval] (length interval))
  where min = minimum [checkGiniSplit trenovaciDataList prah index | prah <- interval]

--Funkce vypočítá GiniSplit hodnotu - podle prahu rozdělí na dvě část, vypočítá GiniLeft a GiniRight 
checkGiniSplit :: [TrenovaciData] -> Float -> Int -> Float
checkGiniSplit trenovaciDataList prah index = fromIntegral (length prvni) / fromIntegral (length trenovaciDataList) * giniLeftRight prvni + fromIntegral (length druhy) / fromIntegral (length trenovaciDataList) * giniLeftRight druhy
  where prvni = fst (splitTrenovaciData trenovaciDataList prah index)
        druhy = snd (splitTrenovaciData trenovaciDataList prah index)

-- Výpočet GiniLeft\GiniRight hodnot, které se použijí výše ve splitu 1 - suma pravdepodobnosti^2
giniLeftRight :: [TrenovaciData] ->  Float
giniLeftRight trenovaciDataList = 1.0 - sum pravdepodobnosti_na2
  where pocty_trid = map length $ group $ sort $ map trida trenovaciDataList -- pocet jednotlivych trid [A,C,B,A,C,C] -> [A,A,B,C,C,C] -> [AA,B,CCC]-> [2,1,3]
        pravdepodobnosti_na2 = map (\x -> (fromIntegral x / fromIntegral (length trenovaciDataList))^2) pocty_trid --(pocty_trid/celkem_trid)^2 -> pravdepodobnost vyskytu dane tridy
--Rozdělí trenovací data na 2 skupiny podle toho, jestli jsou hodnoty indexu na daném řádku větší/menší než práh
splitTrenovaciData :: [TrenovaciData] -> Float -> Int -> ([TrenovaciData],[TrenovaciData])
splitTrenovaciData trenovaciDataList prah index = partition (\x -> floats x !! index <= prah) trenovaciDataList

main :: IO()
main = do
    args <- getArgs
    let (file1, file2) = fileExtract args
    if argsChecker args then
       if file2 /= "" then do --1 podukol 
       file1_content <- readTreeInputAndParse (fst $ fileExtract args)
       file2_content <- readFile (snd $ fileExtract args)
       putStr .  unlines $ prochazejData file1_content $ fileStringToFloats (lines file2_content)
      else do --2 podukol
        file2_content <- readFile (fst $ fileExtract args)
        let nactenaTrenovaciData =  map splitFloatsAndClass (lines file2_content)
        printTree (vytvarejStrom nactenaTrenovaciData) 0

    else handleError "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    return ()

-- main :: IO()
-- main = do
--   args <- getArgs
--   let (file1, file2) = fileExtract args
--   if argsChecker args then
--     if file2 /= "" then --1 podukol
--       readTreeInputAndParse file1 >>= \file1_content ->
--       readFile file2 >>= (\file2_content -> putStr .  unlines $ prochazejData file1_content $ fileStringToFloats (lines file2_content))
--     else --2 podukol
--       readFile (fst $ fileExtract args) >>= (\file_content -> printTree (vytvarejStrom (extractData (lines file_content))) 0)
--   else handleError "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahuj<soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
--   return ()