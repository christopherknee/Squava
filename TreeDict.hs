-- Borrowed from the CPSC 312 TreeDict.hs

module TreeDict
  (Dict,
   emptyDict,  -- Dict k v
   getval,     -- (Ord k) => k -> Dict k v -> Maybe v
   insertval,  -- (Ord k) => k -> v -> Dict k v -> Dict k v
   update_tree, -- (Ord k) => k -> (Maybe v -> v) -> Dict k v -> Dict k v
   tolist,     --  Dict k v -> [(k,v)]
   stats,       -- Dict t1 t2 -> [Char]
   show
   ) where

-- To run it, try:
-- ghci
-- :load TreeDict

-- a binary search tree where k is the type of key, and v is type of value
data BSTree k v = BSEmpty | BSNode k v (BSTree k v) (BSTree k v)
 --      deriving (Show)
instance (Show k, Show v) => Show (BSTree k v) where
   show BSEmpty = ""
   show (BSNode k v t1 t2) = show t1 ++ show k ++ ": " ++ show v ++ "\n" ++ show t2

type Dict = BSTree

emptyDict :: Dict k v
emptyDict = BSEmpty

test_tree = BSNode 10 "fun" (BSNode 3 "wow" BSEmpty BSEmpty) (BSNode 20 "yes" BSEmpty BSEmpty)

-- getval k t   = value associated with key k in tree t
getval :: (Ord k) => k -> Dict k v -> Maybe v
getval key (BSNode kt val l r)
    | key == kt    = Just val
    | key < kt    = getval key l
    | otherwise = getval key r
getval _ BSEmpty = Nothing

-- insertval key v t  returns new tree
insertval :: (Ord k) => k -> v -> Dict k v -> Dict k v
insertval key val BSEmpty = BSNode key val BSEmpty BSEmpty
insertval key val (BSNode kt vt l r)
     | key==kt = BSNode kt val l r   -- replace value
     | key<kt  = BSNode kt vt (insertval key val l) r
     | otherwise  = BSNode kt vt l (insertval key val r)

update_tree:: (Ord k) => k -> (Maybe v -> v) -> Dict k v -> Dict k v
-- update_tree key fun dict = new tree with value for key updated by fun value
update_tree key fun BSEmpty = BSNode key (fun Nothing) BSEmpty BSEmpty
update_tree key fun (BSNode kt vt l r)
     | key==kt = BSNode kt (fun (Just vt)) l r   -- replace value
     | key<kt  = BSNode kt vt (update_tree key fun l) r
     | otherwise  = BSNode kt vt l (update_tree key fun r)

-- tolist tree = list representation of tree
tolist BSEmpty = []
tolist (BSNode key val l r) = tolist l ++ [(key,val)] ++ tolist r

-- tolist ( insertval 77 "test" test_tree)

-- tsize tree =  number of nodes in the tree
tsize tree = length (tolist tree)

--- tdepth tree =  depth of the tree
tdepth  BSEmpty = 0
tdepth (BSNode key val l r) = 1+ max (tdepth l)  (tdepth r)

-- stats tree = some statistics on tree
stats :: Dict t1 t2 -> [Char]
stats tr = "Number of elements="++show (tsize tr)++", Depth="++show (tdepth tr)


