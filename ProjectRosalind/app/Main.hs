import NucleicAcid
import Peptide
import Misc
import FASTA
import Uniprot

import System.IO
import Control.Monad.Reader
import qualified System.IO.Strict as Strict
import Data.List

main :: IO ()
main = do
    table <- loadCodonTable
    let acid = toRna "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG"
    let rnaCombs = [acid, (reverse acid), (comp acid), (reverse $ comp acid)]
    let codons = sequence $ map sequence $ map (\x -> map stringToCodon x) $ map (\x -> chunkList x 3) rnaCombs
    let orfs = (map getOpenReadingFrames) <$> codons
    let f1 = head <$> orfs
    let p = runReader $ 
        where comp x = map alt x
              toRna xs = map (\x -> if x == 'T' then 'U' else x) xs
              alt n = case n of 
                'A' -> 'U'
                'G' -> 'C'
                'C' -> 'G'
                'U' -> 'A'
                otherwise -> 'Z'

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