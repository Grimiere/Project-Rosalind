import NucleicAcid
import Peptide
import Misc
import FASTA

import System.IO
import qualified System.IO.Strict as Strict
import Data.List

main :: IO ()
main = do
    let motif = generatePeptideMotif "N{P}[ST]{P}"
    input <- readFile "input.txt"
    let peptide = getPeptide <$> (head $ stringToPeptideFASTAS input)
    let locs = peptideMotifLocations <$> peptide <*> (pure motif) <*> (pure motif) <*> (pure 1) <*> (pure [])
    print locs

--Wants: Just [Rule a]
--Have: [Just $ Rule a]

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
writeResults h str = hPutStrLn h str >> hClose h

getInput :: IO (String)
getInput = sanitize <$> readFile "input.txt"

tidyList :: (Show a) => [a] -> String
tidyList [] = ""
tidyList [x] = (show x)
tidyList (x:xs) = (show x) ++ " " ++ (tidyList xs)