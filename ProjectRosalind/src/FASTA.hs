module FASTA (
    FASTA,
    createNucleicFASTA,
    createPeptideFASTA,
    getDNA,
    getPeptide,
    getHeader,
    stringToNucleicFASTAS,
    stringToPeptideFASTAS,
    highestGCContent,
) where

import NucleicAcid
import Peptide
import Data.List

data FASTA a = FASTA String a deriving (Show, Read, Eq)

createNucleicFASTA :: String -> NucleicAcid -> FASTA NucleicAcid
createNucleicFASTA str xs = FASTA str xs

createPeptideFASTA :: String -> Peptide -> FASTA Peptide
createPeptideFASTA str xs = FASTA str xs

getDNA :: (FASTA NucleicAcid) -> NucleicAcid
getDNA (FASTA _ dna) = dna

getPeptide :: (FASTA Peptide) -> Peptide
getPeptide (FASTA _ p) = p

getHeader :: FASTA a -> String
getHeader (FASTA str _) = str

isFASTAHeader :: String -> Bool
isFASTAHeader [] = False
isFASTAHeader (x:xs) = x == '>'

stringToNucleicFASTAS :: String -> [Maybe (FASTA NucleicAcid)]
stringToNucleicFASTAS xs = tail $ nucleicFASTAS' (lines xs) [] ([], Just [])

nucleicFASTAS' :: [String] -> [Maybe (FASTA NucleicAcid)] -> (String, Maybe NucleicAcid) -> [Maybe (FASTA NucleicAcid)]
nucleicFASTAS' [] c (str, dna) = (c ++ [(FASTA str) <$> dna])
nucleicFASTAS' [x] c (str, dna) = if (isFASTAHeader x) then [] else (c ++ [(FASTA str) <$> ((++) <$> dna <*> stringToNucleic x)])
nucleicFASTAS' (x:xs) c (str, dna) = if isFASTAHeader x 
                                        then nucleicFASTAS' xs (c ++ [(FASTA str) <$> dna]) (x, Just []) 
                                        else nucleicFASTAS' xs c (str, ((++) <$> dna <*> stringToNucleic x))

stringToPeptideFASTAS :: String -> [Maybe (FASTA Peptide)]
stringToPeptideFASTAS xs = tail $ peptideFASTAS' (lines xs) [] ([], Just [])

peptideFASTAS' :: [String] -> [Maybe (FASTA Peptide)] -> (String, Maybe Peptide) -> [Maybe (FASTA Peptide)]
peptideFASTAS' [] c (str, p) = (c ++ [(FASTA str) <$> p])
peptideFASTAS' [x] c (str, p) = if (isFASTAHeader x) then [] else (c ++ [(FASTA str) <$> ((++) <$> p <*> stringToPeptide x)])
peptideFASTAS' (x:xs) c (str, p) = if isFASTAHeader x 
                                        then peptideFASTAS' xs (c ++ [(FASTA str) <$> p]) (x, Just []) 
                                        else peptideFASTAS' xs c (str, ((++) <$> p <*> stringToPeptide x))

highestGCContent :: [(FASTA NucleicAcid)] -> Maybe (String, Double)
highestGCContent [] = Nothing
highestGCContent [x] = Just (getHeader x, gcContent $ getDNA x)
highestGCContent xs = Just (getHeader highest, (gcContent $ getDNA highest))
    where highest = last $ sortBy (\a b -> ((gcContent $ getDNA a) `compare` (gcContent $ getDNA b))) xs

