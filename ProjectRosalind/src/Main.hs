import NucleicAcid
import Peptide
import Misc

import System.IO
import qualified System.IO.Strict as Strict
import Data.List

main :: IO ()
main = do
    table <- loadCodonTable
    peptide <- stringToPeptide <$> getInput
    case peptide of
        Nothing -> putStrLn "Invalid peptide." >> return ()
        Just p -> let result = (product $ map (\a -> toInteger $ length $ getAminoCodons a table) (p ++ [STOP])) :: Integer in
            print $  result `mod` 1000000

    putStrLn "Finished."
    return ()

loadCodonTable :: IO (CodonTable)
loadCodonTable = do
    cHandle <- openFile "CodonTable.txt" ReadMode
    cContents <- lines <$> Strict.hGetContents cHandle
    let table = createCodonTable cContents
    hClose cHandle
    return table

writeResults :: String -> IO ()
writeResults = writeFile "results.txt"

getInput :: IO (String)
getInput = sanitize <$> readFile "input.txt"