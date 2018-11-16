import NucleicAcid
import Peptide
import Misc
import FASTA
import Uniprot

import System.IO
import qualified System.IO.Strict as Strict
import Data.List

main :: IO ()
<<<<<<< HEAD
main = undefined
    
=======
main = do
    let motif = generatePeptideMotif "N{P}[ST]{P}"
    input <- lines <$> readFile "input.txt"
    fastas <- sequence <$> mapM idToPeptideFASTA input
    case fastas of 
        Nothing -> print "???" >> return ()
        Just xs -> do
            let locs = map (filter (/= '\\')) $ map tidyList (map (\f -> (findPeptideMotif (getPeptide f) motif)) xs)
            let pairs = zip (map (filter (/= '\\')) input) locs
            let out = (map show pairs)
            mapM_ print out

>>>>>>> c529164362c8f2296187db5c1a05cd66bb8f4966
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