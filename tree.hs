--Vojtěch Šíma, xsimav01, 2024
-- FLP Funkcionalni projekt
import System.Environment (getArgs)
import System.IO (openFile, ReadMode, hGetContents)

--Kontrola správně zadaných argumentů
argsChecker ::  [String] -> Bool
argsChecker [x,_] = x == "-2" 
argsChecker [x,_,_] = x == "-1"
argsChecker _  = False

fileExtract :: [String] -> (String, String)
fileExtract [_, file] = (file, "")
fileExtract [_, file1, file2] = (file1,file2)
fileExtract [_] = ("", "")


main :: IO()
main = do
    args <- getArgs
    if (argsChecker args) then do 
        let (file1, file2) = fileExtract args 
    else do
        putStr "Chyba pri spousteni projektu! Jedine mozne formy jsou: \n flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> \n flp-fun -2 <soubor obsahujici trenovaci data> "
    
    return ()
