module NucleicAcid (
    Nucleotide (..),
    NucleicAcid,
    FASTA,
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
    stringToFASTAS,
    highestGCContent
) where 

import Data.Char
import Data.List
import System.IO

data Nucleotide = A | C | G | T | U deriving (Show,Read, Eq)
type NucleicAcid = [Nucleotide]
data FASTA = FASTA String NucleicAcid deriving (Show, Read, Eq)

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

stringToFASTAS :: String -> [Maybe FASTA]
stringToFASTAS = (map stringsToFASTA) . prepFASTAFormat . lines

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

isFASTAHeader :: String -> Bool
isFASTAHeader [] = False
isFASTAHeader (x:xs)
    | x == '>' = True
    | otherwise = False

isNucleic :: String -> Bool
isNucleic str = case (stringToNucleic str) of
                    Nothing -> False
                    Just x -> True
{-
highestGCContent :: [FASTA] -> Maybe (String, Double)
highestGCContent [] = Nothing
highestGCContent fastas = Just $ highestGCContent' fastas (head fastas)
-}

highestGCContent :: [FASTA] -> (String, Double)
highestGCContent xs = (getHeader highest, 100 * (gcContent $ getDNA highest))
    where highest = last $ sortBy (\a b -> ((gcContent $ getDNA a) `compare` (gcContent $ getDNA b))) xs

getDNA :: FASTA -> NucleicAcid
getDNA (FASTA _ dna) = dna

getHeader :: FASTA -> String
getHeader (FASTA str _) = str

--TODO: Clean this up.
{- 
highestGCContent' :: [FASTA] -> FASTA -> (String, Double)
highestGCContent' [] (FASTA id nucleic) = (id, 100 * (getGCContent nucleic)) 
highestGCContent' ((FASTA id nucleic):xs) (FASTA idH nucleicH)
    | current > high = highestGCContent' xs (FASTA id nucleic)
    | otherwise = highestGCContent' xs (FASTA idH nucleicH)
    where current = gcContent nucleic
          high = gcContent nucleicH
-}
