module UnorderedMap (
    UnorderedMap,
    hasElement,
    empty,
    singleton,
    insert,
    toList
) where

data UnorderedMap k v = UnorderedMap [(k, v)] deriving (Show, Read, Eq)

testMap :: UnorderedMap Int Char
testMap = UnorderedMap [(0,'Z'), (1,'O'), (3, 'E'), (4, 'L')]

hasElement :: (Eq k) => UnorderedMap k v -> k -> Bool
hasElement (UnorderedMap []) _ = False
hasElement (UnorderedMap (x:xs)) k  = if k == (fst x) then True
                                      else hasElement (UnorderedMap xs) k 
        
empty :: UnorderedMap k v
empty = UnorderedMap []
    
singleton :: k -> v -> UnorderedMap k v
singleton k v = UnorderedMap [(k,v)]

insert :: (Eq k) => (k, v) -> UnorderedMap k v -> Maybe (UnorderedMap k v)
insert _ (UnorderedMap []) = Nothing
insert (k, v) m
    | hasElement m k = Nothing
    | otherwise = Just (UnorderedMap $ (toList m) ++ [(k,v)])

toList :: UnorderedMap k v -> [(k,v)]
toList (UnorderedMap l) = l