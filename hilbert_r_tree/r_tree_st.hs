{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ForeignFunctionInterface #-}

import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Algorithms.Search as S
import qualified Data.Vector.Algorithms.Insertion as I
import Control.Monad
import Data.Ord
import Control.Monad.ST
import Foreign.C

numSiblings = 2
maxEntries = 5
maxSideLen = 65536 -- must be a power of 2


data NodeEntry s = NodeEntry {mbr :: Rect, lhv :: HilbertValue, childTree :: HRT s}
data LeafEntry = LeafEntry {br :: Rect, hv :: HilbertValue}

data HRT s = Leaf (V.STVector s LeafEntry) | Node (V.STVector s (NodeEntry s))
data PathEntry s = PathEntry {node :: HRT s, index :: Int}

data Rect = Rect { lowX :: Int
            , highX :: Int
            , lowY :: Int
            , highY :: Int }

newtype HilbertValue = HilbertValue Int deriving (Eq, Ord)


foreign import ccall "hilbert_value.c xy2d" c_hilbert_value :: CInt -> CInt -> CInt -> CInt 
findHilbertValue :: Rect -> HilbertValue
findHilbertValue rect = HilbertValue . fromIntegral $ c_hilbert_value maxSideLen (fromIntegral xCenter) (fromIntegral yCenter)
            where xCenter = (lowX rect + highX rect) `div` 2
                  yCenter = (lowY rect + highY rect) `div` 2

intersects :: Rect -> Rect -> Bool
intersects r1 r2 =    lowX r1 < highX r2 && lowX r2 < highX r1
                   && lowY r1 < highY r2 && lowY r2 < highY r1

search :: HRT s -> Rect -> ST s [Rect]
search (Leaf entries) query = (liftM $ filter (intersects query)) (mapSTVector br entries) 
search (Node entries) query = do
                                intersectingChildren <- (liftM $ map childTree) $ filterSTVector (intersects query . mbr) entries
                                results <- mapM (`search` query) intersectingChildren
                                return $ concat results

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
                                                                 chooseLeafWithPath h (childTree nextNode) (PathEntry n nodeIndex:pathEntries)

insertIntoLeaf :: LeafEntry -> PathEntry s -> ST s Bool
insertIntoLeaf newEntry (PathEntry leaf index) | V.length vect >= maxEntries = return False
                                               | otherwise                    = vectorInsert newEntry index (V.length vect) >> return True
                    where vectorInsert value ind end | end == ind = V.write vect ind value
                                                     | otherwise  = V.swap vect end (end-1) >> vectorInsert value ind (end-1)
                          vect = case leaf of Leaf entries -> entries

handleOverflow :: LeafEntry -> PathEntry s -> PathEntry s -> ST s ()
handleOverflow newEntry (PathEntry _ leafIndex) (PathEntry parent _) = undefined

adjustTree :: [PathEntry s] -> ST s (HRT s)
adjustTree ((PathEntry currentNode@(Node childEntries) index):rest) = do
                                                changedEntry <- V.read childEntries index
                                                fixedEntry <- fixNodeEntry changedEntry
                                                V.write childEntries index fixedEntry
                                                I.sortBy (comparing lhv) childEntries
                                                if null rest then return currentNode else adjustTree rest
    where fixNodeEntry node@(NodeEntry _ _ (Node entries)) = do
                                                                newMBR <- (liftM findMBR) (mapSTVector mbr entries)
                                                                newLHV <- (liftM findLHV) (mapSTVector lhv entries)
                                                                return $ node {mbr = newMBR, lhv = newLHV}

findMBR :: [Rect] -> Rect
findMBR rects = Rect lowestX highestX lowestY highestY
    where lowestX  = minimum $ map lowX rects
          highestX = maximum $ map highX rects
          lowestY  = minimum $ map lowY rects
          highestY = maximum $ map highY rects
findLHV :: [HilbertValue] -> HilbertValue
findLHV lhvs = maximum lhvs

mapSTVector :: (a -> b) -> V.STVector s a -> ST s [b]
mapSTVector f v = mapM ((liftM f) . V.read v) [1..V.length v]

filterSTVector :: (a -> Bool) -> V.STVector s a -> ST s [a]
filterSTVector p v = liftM (filter p) $ mapSTVector id v
