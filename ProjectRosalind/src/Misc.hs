module Misc (
    Motif (..),
    MotifRule (..),
    maybeHead,
    maybeLast,
    unconcat,
    sanitize,
) where

import Data.List

--N{P}[ST] = Always N, Except [P], Either [S, T] -- [MotifRule (Maybe AminoAcid)]
type Motif a = [(MotifRule a)]
data MotifRule a = Always a  | Either [a] | Except [a]  deriving (Show, Read, Eq)

instance Functor MotifRule where
    fmap f (Always a) = Always $ f a
    fmap f (Either a) = Either $ map f a
    fmap f (Except a) = Except $ map f a

getMotifLength :: Motif a -> Int
getMotifLength xs = length xs

sanitize :: String -> String
sanitize = filter (/= ' ') . filter (/= '\0') . filter (/= '\n')

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead [x] = Just x
maybeHead (x:xs) = Just x

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (x:xs) = maybeLast xs

unconcat :: [a] -> [[a]]
unconcat [] = []
unconcat (x:xs) = [[x]] ++ unconcat xs

population :: (Int, Int) -> Int -> Int -> Int
population (n, k) adults children
    | n == 1 = adults + children
    | otherwise = population (n-1, k) (adults + children) (k * adults)