module Misc (
    unconcat,
    sanitize
) where

sanitize :: String -> String
sanitize = filter (/= '\0') . filter (/= '\n')

unconcat :: [a] -> [[a]]
unconcat [] = []
unconcat (x:xs) = [[x]] ++ unconcat xs

population :: (Int, Int) -> Int -> Int -> Int
population (n, k) adults children
    | n == 1 = adults + children
    | otherwise = population (n-1, k) (adults + children) (k * adults)