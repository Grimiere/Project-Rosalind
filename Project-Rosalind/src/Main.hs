import NucleicAcid
import Peptide
import Misc

import System.IO
import Data.List

main :: IO ()
main = do
    cHandle <- openFile "CodonTable.txt" ReadMode
    cContents <- init <$> lines <$> hGetContents cHandle
    rHandle <- openFile "rna.txt" ReadMode
    rContents <- init <$> hGetContents rHandle

    let table = createCodonTable cContents
    let rna = stringToNucleic rContents
    case rna of 
        Nothing -> error "Unable to construct RNA sequence"
        Just n -> let result = rnaToPeptide n table in
                  writeResults (peptideToString result) 
    hClose cHandle
    hClose rHandle
    putStrLn "Finished."
    return ()

writeResults :: String -> IO ()
writeResults = writeFile "results.txt"