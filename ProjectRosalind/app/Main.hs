import NucleicAcid
import Peptide
import Misc

import System.IO
import qualified System.IO.Strict as Strict
import Data.List

main :: IO ()
main = do
    putStrLn "Test."
    line <- getLine
    return ()

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

writeResults :: String -> IO ()
writeResults = writeFile "results.txt"

getInput :: IO (String)
getInput = sanitize <$> readFile "input.txt"