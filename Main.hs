--Vojtěch Šíma, xsimav01, 2023
-- FLP - Knapsack problem
import System.Environment ( getArgs )
import Control.Monad
import Text.Parsec
    ( char, digit, spaces, string, many1, sepEndBy, parse, Parsec )
import Text.Parsec.String ()
import Data.Text (replace, pack, unpack)
import System.Exit (exitSuccess,exitFailure)

--Kontrola argumentů zadávaných na vstupu
argsChecker ::  [String] -> (Bool, String, String)
argsChecker [] = (False,"No arguments", "")
argsChecker [x]
    | x == "-i" = (True, "-i", "stdin")
    | x == "-b" = (True, "-b", "stdin")
    | x == "-o" = (True, "-o", "stdin")
    | otherwise = (False, "Spatny prepinac", "ignore")
argsChecker [x, y]
    | (x == "-i" || x == "-b"|| x == "-o") &&  (y == "-i" || y == "-b"|| y == "-o") = (False, "Dva prepinace", "ignore")
    | x == "-i" = (True, "-i", y)
    | x == "-b" = (True, "-b", y)
    | x == "-o" = (True, "-o", y)
    | y == "-i" = (True, "-i", x)
    | y == "-b" = (True, "-b", x)
    | y == "-o" = (True, "-o", x)
    | otherwise = (False, "Chybne zadane prepinace", "ignore")

argsChecker _ = (False, "Zadano moc  prepinacu (argumentu)", "ignore")

--Jednotlivé itemy, co se nachází v Knapsacku
data Item = Item {
   weight :: Int,
   cost   :: Int
} deriving Show

--Samotný Knapsack s maximální váhou, minimální cenou a itemy co do něho mohou být umístěny
data Knapsack = Knapsack {
   maxWeight :: Int,
   minCost   :: Int,
   items     :: [Item]
} deriving Show

--První funkce na parsování vstupu, projde klíčové slovo Knapsack a vyhledá a uloží maxWeight a min Cost do struktury a zavolý parsování itemů
knapsackParser :: Parsec String () Knapsack
knapsackParser = do
  string "Knapsack {"
  spaces
  maxWeightKP <- parseWeightCostValue "maxWeight"
  minCostKP <- parseWeightCostValue "minCost"
  itemsKP <- parseItems
  spaces
  char '}'
  return $ Knapsack maxWeightKP minCostKP itemsKP

--Funkce na získání hodnoty jednotlivýcho položek podle jména
parseWeightCostValue :: String -> Parsec String () Int
parseWeightCostValue name = do
  string name
  spaces
  char ':'
  spaces
  value <- many1 digit
  spaces
  return $ read value
--Funkce co vyhledá "items" a následně volá zpracování jednotlivých itemů oddělených mezerou
parseItems :: Parsec String () [Item]
parseItems = do
  string "items:"
  spaces
  char '['
  spaces
  itemsPI <- parseItem `sepEndBy` char ' '
  spaces
  char ']'
  return itemsPI

--Zpracování samotných itemů, kdy je konečně uložena weight a cost do struktury
parseItem :: Parsec String () Item
parseItem = do
  string "Item {"
  spaces
  weightPI <- parseWeightCostValue "weight"
  costPI <- parseWeightCostValue "cost"
  spaces
  char '}'
  return $ Item weightPI costPI

-- Nahrazení \n za " " pro lepší zpracování v následném parsování
newlineReplace :: Char -> Char -> String -> String
newlineReplace _ _ [] = []
newlineReplace a b (x : xs)
                  | x == a = b : newlineReplace a b xs
                  | otherwise = x : newlineReplace a b xs

-- Odstranění \n z načteného souboru pro lepší zpracování v parseru
newlineRemover :: String -> String
newlineRemover str =
  let t = pack str
      t' = replace (pack "\r") (pack "") t
  in unpack t'

-- Základní kontrola knapsacku a toho jestli je správně zadán
isKnapsackValid :: Int -> [Item] -> IO ()
isKnapsackValid 0 _ = handleError "maxWeight = 0!"
isKnapsackValid _ itemsKV = when (null (map weight itemsKV)) $ handleError "Prazdna instance Knapsacku!"

--Funkce která mezi sebou vynásobí a sečte 2 listy co jsou na vstup
multipleLists :: [Int] -> [Int] -> Int
multipleLists [] _ = 0
multipleLists _ [] = 0
multipleLists (x:xs) (y:ys) = x*y + multipleLists xs ys

--Generování všech možných řešení pro brute force (2naN)
allPossibleSolutions :: Int -> [t] -> [[t]]
allPossibleSolutions 0 _ = return []
allPossibleSolutions n xs = do
  values <- xs
  recursiveF <- allPossibleSolutions (n-1) xs
  return (values : recursiveF)

--Tisk řešení podle vzoru pokud je nalezeno
printSolution :: (Int, [Int]) -> IO ()
printSolution (_, values) = putStr $ "Solution [" ++ unwords (map show values) ++ "]"

--Následnující 3 funkce slouží pro vzhorový výpis při přepínačí -i
printKnapsack :: Knapsack -> IO ()
printKnapsack (Knapsack maxWeightPK minCostPK itemsPK) = do
    putStrLn "Knapsack {"
    putStrLn $ "maxWeight: " ++  show maxWeightPK
    putStrLn $ "minCost: " ++  show minCostPK
    putStrLn "items: ["
    printItems itemsPK
    putStrLn "]"
    putStr "}"

printItems :: [Item] -> IO ()
printItems  itemsPI = do
  let weights = map weight itemsPI
  let costs = map cost itemsPI
  printValues weights costs

printValues :: [Int] -> [Int] -> IO()
printValues _ [] = return ()
printValues [] _ = return ()
printValues  (x:xs) (y:ys) = do
  putStrLn "    Item {"
  putStrLn $ "    weight: " ++ show x
  putStrLn $ "    cost: " ++ show y
  putStrLn "    }"
  printValues xs ys


--Pokud není nalezeno řešní, je zavolána tato funkce je běh ukončen
emptySolution :: IO()
emptySolution = do
  putStr "False"
  exitSuccess

--Funkce která startuje hledání řešení pomocí bruteforce
bruteforce :: Knapsack -> IO ()
bruteforce (Knapsack maxWeightBF minCostBF itemsBF) = do
    isKnapsackValid maxWeightBF itemsBF
    let weightList = map weight itemsBF
    let costList = map cost itemsBF

    let allSolutionsListWeights = filter (\vec -> multipleLists vec weightList <= maxWeightBF) $ allPossibleSolutions (length weightList) [0,1]
    let allSolutionsListBoth = filter (\vec -> multipleLists vec costList >= minCostBF) allSolutionsListWeights

    let finalCostsList = map (\vec -> (multipleLists vec costList, vec)) allSolutionsListBoth
    when (null finalCostsList) emptySolution

    let solution = foldl1 (\(costl, vecl) (costr, vecr) -> if costl > costr then (costl, vecl) else (costr, vecr))  finalCostsList
    printSolution solution

    exitSuccess

--Optimalizovaná metoda pomocí genetického algoritmu se mi bohužel nepodařila zprovoznit
optimaze :: Knapsack -> IO ()
optimaze (Knapsack maxWeightO minCostO itemsO) = do
  isKnapsackValid maxWeightO itemsO
  emptySolution

--Startovací funkce, která podle přepínačů zavolá správnou funkce
findSolution:: Knapsack -> String -> IO ()
findSolution knapsack prepinac = do
    if prepinac == "-i" then printKnapsack knapsack else (if prepinac == "-b" then bruteforce knapsack else optimaze knapsack)
    exitSuccess

-- Ošetření errorových situací, výpis a ukončení
handleError :: String -> IO()
handleError message = do
    print ("Neuspesny beh programu z duvodu -> " ++ message)
    exitFailure

-- Funkce na získání instance knapsacku z stdin
readFromStdin :: String -> IO ()
readFromStdin prepinac = do
    rawKnapsack <- getContents
    convertInput rawKnapsack prepinac

-- Funkce, kde jsou informace o instanci knapsacku načteny ze souboru zadaného při spuštění
readFromFile :: String -> String -> IO ()
readFromFile filename prepinac = do
    rawKnapsack <- readFile filename
    convertInput rawKnapsack prepinac

-- Funkce na úpravu raw vstupu a převod do Knapsack struktury, která je pak poslána dále
convertInput :: String -> String -> IO ()
convertInput rawKnapsack prepinac = do
    let rawKnapsackNoNewLines = newlineRemover rawKnapsack
    let rawKnapsackNoNewLinesNoR = newlineReplace '\n' ' ' rawKnapsackNoNewLines
    case parse knapsackParser "" rawKnapsackNoNewLinesNoR of
            Left err  -> handleError (show err)
            Right loadedKnapsack  -> findSolution loadedKnapsack prepinac

main :: IO()
main = do
    args <- getArgs
    let (ok, prepinac, file) = argsChecker args
    if ok then return () else handleError prepinac

    if file == "stdin" then readFromStdin prepinac else readFromFile file prepinac

    return ()