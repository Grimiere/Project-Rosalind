module Peptide (
    Peptide,
    AminoAcid (..),
    Codon,
    CodonTable,
    codonToAmino,
    createCodonTable,
    rnaToPeptide,
    peptideToString,
    getAminoCodons,
    stringToPeptide
) where

import qualified NucleicAcid as NA
import Misc (unconcat)
import Data.List
import qualified UnorderedMap as UM
import Text.Read (readMaybe)

type Peptide = [AminoAcid]
data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K |
                 M | F | P | S | T | W | Y | V | STOP deriving (Show, Read, Eq, Enum)
type Codon = (NA.Nucleotide, NA.Nucleotide, NA.Nucleotide)
type CodonTable = UM.UnorderedMap Codon AminoAcid

codonToAmino :: Codon -> CodonTable -> Maybe AminoAcid
codonToAmino codon table = UM.lookup codon table  

createCodonTable :: [String] -> CodonTable
createCodonTable str =  createCodonTable' str UM.empty

createCodonTable' :: [String] -> CodonTable -> CodonTable
createCodonTable' [] carry = carry
createCodonTable' (x:xs) carry = do
    let amino = (read :: String -> AminoAcid) (takeWhile (/= ':') x)
    let codonsStr = words $ drop 1 $ dropWhile(/= ':') x
    let codons = map stringToCodon codonsStr
    let fixed = sequence codons
    case fixed of
        Nothing -> UM.empty
        Just ys -> do
            let new = map (\c -> (c, amino)) ys
            createCodonTable' xs (UM.insertSet new carry) 

rnaToPeptide :: NA.NucleicAcid -> CodonTable -> Peptide
rnaToPeptide [] _ = []
rnaToPeptide rna table = rnaToPeptide' rna table []

rnaToPeptide' :: NA.NucleicAcid -> CodonTable -> Peptide -> Peptide
rnaToPeptide' [] _ carry = carry
rnaToPeptide' l _ carry
            | length l < 3 = carry
rnaToPeptide' (x:y:z:xs) table carry = case amino of
                                       Nothing -> []
                                       Just STOP -> carry --Stop codon reached, no more synthesis.
                                       Just a -> rnaToPeptide' xs table (carry ++ [a])
    where amino = codonToAmino (x,y,z) table

stringToCodon :: String -> Maybe Codon
stringToCodon str
    | length str /= 3 = Nothing
    | otherwise = case result of
                  Nothing -> Nothing
                  Just (x:y:z:xs) -> Just (x, y, z)
    where fixed = unconcat str
          result = sequence $ map (readMaybe :: String -> Maybe NA.Nucleotide) fixed

peptideToString :: Peptide -> String
peptideToString p = concat $ map show p

stringToPeptide :: String -> Maybe Peptide
stringToPeptide [] = Nothing
stringToPeptide str = stringToPeptide' str []

stringToPeptide' :: String -> [Maybe AminoAcid] -> Maybe Peptide
stringToPeptide' [] carry = sequence carry
stringToPeptide' (x:xs) carry = stringToPeptide' xs (carry ++ [readMaybe [x] ])

getAminoCodons :: AminoAcid -> CodonTable -> [Codon]
getAminoCodons amino table = let list = UM.toList table in
                             map fst $ filter (\pair -> (snd pair )== amino) list