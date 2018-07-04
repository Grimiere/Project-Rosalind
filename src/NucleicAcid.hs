module NucleicAcid (
    Nucleotide (..),
    stringToNucleic,
    nucleicToString,
    charToNucleotide,
    countNucleotide,
    getNucleotides,
    dnaToRNA,
    getDNACompliment,
    hammingDistance,
) where 

import Data.Char
import Data.List

data Nucleotide = A | C | G | T | U deriving (Show,Read, Eq)
type NucleicAcid = [Nucleotide] 

stringToNucleic :: String -> Maybe NucleicAcid
stringToNucleic [] = Nothing
stringToNucleic str
    | summed < (length str) = Nothing
    | otherwise = sequence $ map charToNucleotide fixed 
    where summed = ac + tc + gc + cc + uc
          fixed = map toUpper str
          nCount = count fixed
          ac = nCount 'A'
          tc = nCount 'T'
          gc = nCount 'G'
          cc = nCount 'C'
          uc = nCount 'U'

nucleicToString :: NucleicAcid -> String
nucleicToString na = concat $ map show na

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide x
    | char == 'A' = Just A
    | char == 'T' = Just T
    | char == 'G' = Just G
    | char == 'C' = Just C
    | char == 'U' = Just U
    | otherwise = Nothing
    where char = toUpper x

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

getDNACompliment :: NucleicAcid -> NucleicAcid
getDNACompliment = reverse . map deoxyNucleotideComplement

hammingDistance :: NucleicAcid -> NucleicAcid -> Int
hammingDistance n1 n2 =  hammingDistance' n1 n2 0

hammingDistance' :: NucleicAcid -> NucleicAcid -> Int -> Int
hammingDistance' [] _ i = i
hammingDistance' _ [] i = i
hammingDistance' (x:xs) (y:ys) i = if (x /= y) 
                                  then hammingDistance' xs ys (i + 1)
                                  else hammingDistance' xs ys i