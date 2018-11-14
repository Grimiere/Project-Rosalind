module Misc (
    Motif (..),
    MotifRule (..),
    unconcat,
    sanitize,
) where

--N{P}[ST] = Always N, Except [P], Either [S, T] -- [MotifRule (Maybe AminoAcid)]
type Motif a = [(MotifRule a)]
data MotifRule a = Always a  | Either [a] | Except [a]  deriving (Show, Read, Eq)

instance Functor MotifRule where
    fmap f (Always a) = Always $ f a
    fmap f (Either a) = Either $ map f a
    fmap f (Except a) = Except $ map f a

instance Applicative MotifRule where
    pure = Always
    (Always f) <*> (Always a) = Always $ f a
    (Always f) <*> (Either a) = Either $ map f a
    (Always f) <*> (Except a) = Except $ map f a
    (Either a) <*> (Always b) = Either $ a <*> [b] --PATTERN
    (Either a) <*> (Either b) = Either $ a <*> b
    (Either a) <*> (Except b) = Except $ a <*> b
    (Except a) <*> (Always b) = Except $ a <*> [b]
    (Except a) <*> (Either b) = Either $ a <*> b
    (Except a) <*> (Except b) = Except $ a <*> b

getMotifLength :: Motif a -> Int
getMotifLength xs = length xs

sanitize :: String -> String
sanitize = filter (/= ' ') . filter (/= '\0') . filter (/= '\n')

unconcat :: [a] -> [[a]]
unconcat [] = []
unconcat (x:xs) = [[x]] ++ unconcat xs

population :: (Int, Int) -> Int -> Int -> Int
population (n, k) adults children
    | n == 1 = adults + children
    | otherwise = population (n-1, k) (adults + children) (k * adults)