-- Borrowed from the CPSC 312 HashTreeDict.hs
module HashTreeDict
  (Dict,
   emptyDict,  -- Dict k v
   getval,     -- (Hash k, Ord k) => k -> Dict k v -> Maybe v
   insertval,  -- (Hash k, Ord k) => k -> v -> Dict k v -> Dict k v
   tolist,     --  Dict k v -> [(k,v)]
   stats        -- Dict t1 t2 -> [Char]
   ) where

import qualified TreeDict
import Hash

type Dict k v = TreeDict.Dict (Int,k) v
emptyDict = TreeDict.emptyDict
getval k = TreeDict.getval (hash k, k)
insertval k = TreeDict.insertval (hash k, k)
tolist = TreeDict.tolist
stats = TreeDict.stats


