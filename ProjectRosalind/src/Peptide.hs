module Peptide (
    Peptide,
    AminoAcid (..),
    Codon,
    CodonTable,
    MassTable,
    codonToAmino,
    createCodonTable,
    getAminoMass,
    getPeptideMass,
    createMassTable,
    rnaToPeptide,
    peptideToString,
    getAminoCodons,
    stringToPeptide,
    generatePeptideMotif,
    findPeptideMotif,
    peptideMatchesRule,
) where

import qualified NucleicAcid as NA
import Misc
import Data.List
import Control.Monad.Reader
import qualified UnorderedMap as UM
import Text.Read (readMaybe)

type Peptide = [AminoAcid]
data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K |
                 M | F | P | S | T | W | Y | V | STOP deriving (Show, Read, Eq, Enum)
type Codon = (NA.Nucleotide, NA.Nucleotide, NA.Nucleotide)
type CodonTable = UM.UnorderedMap Codon AminoAcid
type MassTable = UM.UnorderedMap AminoAcid Double

codonToAmino :: Codon -> Reader CodonTable (Maybe AminoAcid)
codonToAmino codon = do
    table <- ask
    return $ UM.lookup codon table  

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
        
getAminoMass :: AminoAcid -> Reader MassTable (Maybe Double)
getAminoMass amino = do
    table <- ask 
    return $ UM.lookup amino table

getPeptideMass :: Peptide -> Reader MassTable (Maybe Double)
getPeptideMass xs = do 
    table <- ask
    return $ sum <$> sequence (map (\aa -> runReader (getAminoMass aa) table) xs)

createMassTable :: [String] -> MassTable
createMassTable str = createMassTable' str UM.empty

createMassTable':: [String] -> MassTable -> MassTable
createMassTable' [] carry = carry
createMassTable' (x:xs) carry = do
    let amino = (read :: String -> AminoAcid) (takeWhile (/= ':') x)
    let mass = (read :: String -> Double ) $ sanitize $ drop 1 $ dropWhile (/= ':') x
    createMassTable' xs (UM.insert (amino, mass) carry)

rnaToPeptide :: NA.NucleicAcid -> Reader CodonTable Peptide
rnaToPeptide [] = return []
rnaToPeptide rna = do 
    table <- ask
    return $ rnaToPeptide' rna table []

rnaToPeptide' :: NA.NucleicAcid -> CodonTable -> Peptide -> Peptide
rnaToPeptide' [] _ carry = carry
rnaToPeptide' l _ carry
            | length l < 3 = carry
rnaToPeptide' (x:y:z:xs) table carry = case amino of
                                       Nothing -> []
                                       Just STOP -> carry --Stop codon reached, no more synthesis.
                                       Just a -> rnaToPeptide' xs table (carry ++ [a])
    where amino = runReader (codonToAmino (x,y,z)) table

charToAminoAcid :: Char -> Maybe AminoAcid
charToAminoAcid x = readMaybe [x]

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

getAminoCodons :: AminoAcid -> Reader CodonTable [Codon]
getAminoCodons amino = do 
    table <- ask
    let list = UM.toList table in
        return $ map fst $ filter (\pair -> (snd pair )== amino) list

isAminoAcid :: Char -> Bool
isAminoAcid x = case (charToAminoAcid x) of
    Just _ -> True
    Nothing -> False

generatePeptideMotif :: String -> Motif AminoAcid
generatePeptideMotif [] = []
generatePeptideMotif xs = generatePeptideMotif' xs []

--TODO: Clean this up. No support for N ... Except P.
generatePeptideMotif' :: String -> (Motif AminoAcid) -> (Motif AminoAcid)
generatePeptideMotif' [] carry = carry
generatePeptideMotif' [x] carry = if isAminoAcid x then (carry ++ [Always (getAmino x)]) else []
            where getAmino x = (read ::String -> AminoAcid) [x]
generatePeptideMotif' (x:xs) carry
        | isAminoAcid x = generatePeptideMotif' xs (carry ++ [Always $ getAmino x])
        | otherwise = case x of
            '[' -> let aminos = takeWhile isAminoAcid xs in
                generatePeptideMotif' (drop ((length aminos) + 1) xs) (carry ++ [Either $ map getAmino aminos])
            '{' -> let aminos = takeWhile isAminoAcid xs in
                generatePeptideMotif' (drop ((length aminos) + 1 ) xs) (carry ++ [Except $ map getAmino aminos])
            otherwise -> []
            where getAmino x = (read ::String -> AminoAcid) [x]
        
findPeptideMotif :: Peptide -> (Motif AminoAcid) -> [Int]
findPeptideMotif xs m = foldr (\x acc -> if (peptideMatchesRule (subX x) m) then ((1+x) : acc) else acc) [] [0..(length xs)]
    where subX n = take (length m) $ drop n xs

peptideMatchesRule :: Peptide -> (Motif AminoAcid) -> Bool
peptideMatchesRule [] _ = False
peptideMatchesRule _ [] = False
peptideMatchesRule xs ys
        | (length xs) /= (length ys) = False
        | otherwise = and mapZipped
                where zipped = zip xs ys
                      mapZipped = map (\(a,m) -> aminoMatchesRule a m) zipped

aminoMatchesRule :: AminoAcid -> (MotifRule AminoAcid) -> Bool
aminoMatchesRule a (Always x) = a == x
aminoMatchesRule a (Either xs) = a `elem` xs
aminoMatchesRule a (Except xs) = not $ a `elem` xs




