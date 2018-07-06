module UnorderedMap (
    UnorderedMap (..),
    hasElement,
    empty,
    singleton,
    insert,
    UnorderedMap.lookup,
    toList,
    insertSet,
    union
) where

data UnorderedMap k v = UnorderedMap [(k, v)] deriving (Show, Read, Eq)

testMap :: UnorderedMap Int Char
testMap = UnorderedMap [(0,'Z'), (1,'O'), (2, 'E'), (3, 'L')]

hasElement :: (Eq k) => UnorderedMap k v -> k -> Bool
hasElement (UnorderedMap []) _ = False
hasElement (UnorderedMap (x:xs)) k  = if k == (fst x) then True
                                      else hasElement (UnorderedMap xs) k 
        
empty :: UnorderedMap k v
empty = UnorderedMap []
    
singleton :: (k,v) -> UnorderedMap k v
singleton kv = UnorderedMap [kv]

insert :: (Eq k) => (k, v) -> UnorderedMap k v -> UnorderedMap k v
insert _ (UnorderedMap []) = UnorderedMap.empty
insert (k, v) m
    | hasElement m k = UnorderedMap.empty
    | otherwise = UnorderedMap $ (toList m) ++ [(k,v)]

insertSet :: (Eq k) => [(k,v)] -> UnorderedMap k v -> UnorderedMap k v
insertSet [] m = m
insertSet set m = let newMap = UnorderedMap set in
                  m `union` newMap
    
union :: UnorderedMap k v -> UnorderedMap k v -> UnorderedMap k v
union m1 m2 = UnorderedMap $ (toList m1) ++ (toList m2)

lookup :: (Eq k) => k -> UnorderedMap k v -> Maybe v
lookup _ (UnorderedMap []) = Nothing
lookup k (UnorderedMap ((a,b):xs))
    | a == k = Just b
    | otherwise = UnorderedMap.lookup k (UnorderedMap xs)


toList :: UnorderedMap k v -> [(k,v)]
toList (UnorderedMap l) = l