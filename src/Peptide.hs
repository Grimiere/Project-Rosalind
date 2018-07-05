module Peptide (

) where

import qualified NucleicAcid as NA
import Misc (unconcat)
import Data.List
import qualified UnorderedMap as Map
import Text.Read (readMaybe)

type Peptide = [AminoAcid]
data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K |
                 M | F | P | S | T | W | Y | V | STOP deriving (Show, Read, Eq, Enum, Hashable)
type Codon = (NA.Nucleotide, NA.Nucleotide, NA.Nucleotide)
type CodonTable =  Codon AminoAcid

codonToAmino :: Codon -> CodonTable -> Maybe AminoAcid
codonToAmino codon table = Map.lookup codon table  

createCodonTable :: [String] -> CodonTable
createCodonTable str = undefined

stringToCodon :: String -> Maybe Codon
stringToCodon str
    | length str /= 3 = Nothing
    | otherwise = case result of
                  Nothing -> Nothing
                  Just (x:y:z:xs) -> Just (x, y, z)
    where fixed = unconcat str
          result = sequence $ map (readMaybe :: String -> Maybe NA.Nucleotide) fixed