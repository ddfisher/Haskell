{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Algorithms.Search as S
import Data.Ord
import Control.Monad.ST

num_siblings = 2
max_entries = 5

data NodeEntry s = NodeEntry {mbr :: Rect, lhv :: HilbertValue, child_tree :: HRT s}
data LeafEntry = LeafEntry {br :: Rect, hv :: HilbertValue}

data HRT s = Leaf (V.STVector s LeafEntry) | Node (V.STVector s (NodeEntry s))
data PathEntry s = PathEntry {node :: HRT s, index :: Int}

data Rect = Rect { lowX :: Int
            , highX :: Int
            , lowY :: Int
            , highY :: Int }

newtype HilbertValue = HilbertValue Int deriving (Eq, Ord)

insert :: HRT s -> Rect -> ST s (HRT s)
insert tree newRect = do
        let h = findHilbertValue newRect
        let newEntry = LeafEntry newRect h
        (leaf, parents) <- chooseLeaf h tree
        full <- insertIntoLeaf newEntry leaf
        if full then handleOverflow newEntry leaf (head parents) else undefined --XXX x_x
        adjustTree parents

chooseLeaf :: HilbertValue -> HRT s -> ST s (PathEntry s, [PathEntry s])
chooseLeaf h tree = chooseLeafWithPath h tree []
      where chooseLeafWithPath h l@(Leaf entries) pathEntries = do
                                                                 leafIndex <- S.binarySearchBy (comparing hv) entries LeafEntry {hv = h}
                                                                 return (PathEntry l leafIndex, pathEntries)
            chooseLeafWithPath h n@(Node entries) pathEntries = do
                                                                 nodeIndex <- S.binarySearchBy (comparing lhv) entries NodeEntry {lhv = h}
                                                                 nextNode <- V.read entries nodeIndex 
                                                                 chooseLeafWithPath h (child_tree nextNode) (PathEntry n nodeIndex:pathEntries)

insertIntoLeaf :: LeafEntry -> PathEntry s -> ST s Bool
insertIntoLeaf newEntry (PathEntry leaf index) | V.length vect >= max_entries = return False
                                               | otherwise                    = vectorInsert newEntry index (V.length vect) >> return True
                    where vectorInsert value ind end | end == ind = V.write vect ind value
                                                     | otherwise  = V.swap vect end (end-1) >> vectorInsert value ind (end-1)
                          vect = case leaf of Leaf entries -> entries

handleOverflow :: LeafEntry -> PathEntry s -> PathEntry s -> ST s ()
handleOverflow newEntry (PathEntry _ leafIndex) (PathEntry parent _) = undefined

adjustTree :: [PathEntry s] -> ST s (HRT s)
adjustTree = undefined

findHilbertValue :: Rect -> HilbertValue
findHilbertValue = undefined --TODO: complete

