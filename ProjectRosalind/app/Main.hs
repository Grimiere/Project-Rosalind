import NucleicAcid
import Peptide
import Misc

import System.IO
import qualified System.IO.Strict as Strict
import Data.List

main :: IO ()
main = do
    dna <- stringToNucleic <$> getInput
    let count = getNucleotides <$> dna
    case count of
        Just tuple -> print tuple
        Nothing -> putStrLn "Invalid data entered."

loadCodonTable :: IO (CodonTable)
loadCodonTable = do
    cHandle <- openFile "CodonTable.txt" ReadMode
    cContents <- lines <$> Strict.hGetContents cHandle
    let table = createCodonTable cContents
    hClose cHandle
    return table

loadMassTable :: IO (MassTable)
loadMassTable = do
    mHandle <- openFile "MassTable.txt" ReadMode
    mContents <- lines <$> Strict.hGetContents mHandle
    let table = createMassTable mContents
    hClose mHandle
    return table

writeResults :: Handle -> String -> IO ()
writeResults h str = hPutStrLn h str

getInput :: IO (String)
getInput = sanitize <$> readFile "input.txt"

tidyList :: (Show a) => [a] -> String
tidyList [] = ""
tidyList [x] = (show x)
tidyList (x:xs) = (show x) ++ " " ++ (tidyList xs)