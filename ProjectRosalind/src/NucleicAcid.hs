module NucleicAcid (
    Nucleotide (..),
    NucleicAcid,
    stringToNucleic,
    nucleicToString,
    charToNucleotide,
    countNucleotide,
    getNucleotides,
    gcContent,
    dnaToRNA,
    getDNAComplement,
    deoxyNucleotideComplement,
    hammingDistance,
    getMotifLocations
) where 

import Data.Char
import Data.List
import System.IO

data Nucleotide = A | C | G | T | U deriving (Show,Read, Eq)
type NucleicAcid = [Nucleotide]

--TODO: Reimplement this.
stringToNucleic :: String -> Maybe NucleicAcid
stringToNucleic = undefined

nucleicToString :: NucleicAcid -> String
nucleicToString na = concat $ map show na

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide = undefined

count :: (Eq a) => [a] -> a -> Int
count l a = length $ a `elemIndices` l

countNucleotide :: NucleicAcid -> Nucleotide -> Int
countNucleotide na n = count na n

getNucleotides :: NucleicAcid -> (Int, Int, Int, Int, Int)
getNucleotides na = (cn A, cn C, cn G, cn T, cn U)
    where cn = countNucleotide na

dnaToRNA :: NucleicAcid -> NucleicAcid
dnaToRNA na = map (\n -> if n == T then U else n) na

deoxyNucleotideComplement :: Nucleotide -> Nucleotide
deoxyNucleotideComplement A = T
deoxyNucleotideComplement T = A
deoxyNucleotideComplement C = G
deoxyNucleotideComplement G = C

getDNAComplement :: NucleicAcid -> NucleicAcid
getDNAComplement = map deoxyNucleotideComplement

hammingDistance :: NucleicAcid -> NucleicAcid -> Int
hammingDistance n1 n2 =  hammingDistance' n1 n2 0

hammingDistance' :: NucleicAcid -> NucleicAcid -> Int -> Int
hammingDistance' [] _ i = i
hammingDistance' _ [] i = i
hammingDistance' (x:xs) (y:ys) i = if (x /= y) 
                                  then hammingDistance' xs ys (i + 1)
                                  else hammingDistance' xs ys i

gcContent :: NucleicAcid -> Double
gcContent na = 100 * (fromIntegral(c + g)) / (fromIntegral(length na))
    where c = countNucleotide na C
          g = countNucleotide na G
                                        
{-
stringsToFASTA :: [String] -> Maybe FASTA
stringsToFASTA [] = Nothing
stringsToFASTA (x:xs)
    | head x == '>' = let nucleic = stringToNucleic (concat xs) in
                      case nucleic of
                        Nothing -> Nothing
                        Just na -> Just (FASTA x na)
    | otherwise = Nothing

prepFASTAFormat :: [String] -> [[String]]
prepFASTAFormat [] = []
prepFASTAFormat input = prepFASTAFormat' input []

prepFASTAFormat' :: [String] -> [[String]] -> [[String]]
prepFASTAFormat' [] carry = carry
prepFASTAFormat' (x:xs) carry
    | isFASTAHeader x = let nucleics = takeWhile isNucleic xs in
                        prepFASTAFormat' xs (carry ++ [[x] ++ nucleics])
    | otherwise = prepFASTAFormat' xs carry
-}

getMotifLocations :: NucleicAcid -> NucleicAcid -> [Int]
getMotifLocations [] _ = []
getMotifLocations _ [] = []
getMotifLocations xs ys
    | (length ys) > (length xs) = []
    | otherwise = getMotifLocations' xs ys [] 1

getMotifLocations' :: NucleicAcid -> NucleicAcid -> [Int] -> Int -> [Int]
getMotifLocations' [] _ carry _ = carry
getMotifLocations' _ [] carry _ = carry
getMotifLocations' xs ys carry i = do
    let subStr = take (length ys) xs
    let newCarry = if subStr == ys then carry ++ [i] else carry in 
        getMotifLocations' (tail xs) ys newCarry (i+1)
  