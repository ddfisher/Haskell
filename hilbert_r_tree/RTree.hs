{-# LANGUAGE ViewPatterns #-}
module RTree (empty, search, RTree.insert, size, Rect(..), maxSideLen, intersects) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Foreign.C
import Control.Monad
import Control.DeepSeq

numSiblings = 2
maxEntries = 10
maxSideLen = 65536 :: Int -- must be a power of 2

-- In leaf nodes, all the childTrees are End
data Entry = Entry {mbr :: Rect, lhv :: HilbertValue, childTree :: HRT}
    deriving (Eq, Show)
data HRT = Node [Entry] | Leaf [Entry] | Stub
    deriving (Eq, Show)

instance NFData Entry where
    rnf (Entry a b c) = a `deepseq` b `deepseq` c `deepseq` ()

instance NFData HRT where
    rnf (Node lst) = lst `deepseq` ()
    rnf (Leaf lst) = lst `deepseq` ()
    rnf Stub       = ()

getEntries :: HRT -> [Entry]
getEntries (Leaf entries) = entries
getEntries (Node entries) = entries
getEntries Stub = error "Should not get Stub entries"

rewrap :: HRT -> [Entry] -> HRT
rewrap (Leaf _) = Leaf
rewrap (Node _) = Node

empty = Leaf []

data Rect = Rect { lowX :: Int
            , highX :: Int
            , lowY :: Int
            , highY :: Int }
    -- deriving Eq
    deriving (Eq, Show)

instance NFData Rect where
    rnf (Rect a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

-- instance Show Rect where
--     show (Rect lX hX lY hY) = printf "%d,%d,%d,%d,%d,%d,%d,%d" hX hY hX lY lX lY lX hY

instance Read Rect where
    readsPrec _ str = [(buildRect $ map read $ splitOn "," str, "")]
        where buildRect [x1, y1, x2, y2, x3, y3, x4, y4] = Rect minX maxX minY maxY 
                where maxX = maximum [x1,x2,x3,x4]
                      minX = minimum [x1,x2,x3,x4]
                      maxY = maximum [y1,y2,y3,y4]
                      minY = minimum [y1,y2,y3,y4]
buildRect _ = error "Couldn't build rect"

newtype HilbertValue = HilbertValue Int deriving (Eq, Ord, Show)
instance NFData HilbertValue where
    rnf (HilbertValue v) = rnf v

foreign import ccall "hilbert_value.c xy2d" c_hilbert_value :: CInt -> CInt -> CInt -> CInt 
findHilbertValue :: Rect -> HilbertValue
findHilbertValue rect = HilbertValue . fromIntegral $ c_hilbert_value (fromIntegral maxSideLen) (fromIntegral xCenter) (fromIntegral yCenter)
            where xCenter = (lowX rect + highX rect) `div` 2
                  yCenter = (lowY rect + highY rect) `div` 2

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ []     = []
deleteIndex 0 (x:xs) = xs
deleteIndex i (x:xs) = x:deleteIndex (i-1) xs 

-- R Tree Specific Functions

size :: HRT -> Integer
size Stub                    = 1
size (getEntries -> entries) = sum $ map (size.childTree) entries

search :: HRT -> Rect -> [Rect]
search (Leaf entries) rect = filter (intersects rect) (map mbr entries)
search (Node entries) rect = do
            entry <- entries
            if mbr entry `intersects` rect
                then search (childTree entry) rect
                else []

insert :: Rect -> HRT -> HRT
insert r tree = findRoot . fixNode . hilbertInsert tree $ entry
        where findRoot [root] = root
              findRoot nodes  = Node $ map makeEntry nodes
              entry           = Entry r (findHilbertValue r) Stub

hilbertInsert :: HRT -> Entry -> HRT
hilbertInsert (Node entries) newEntry = Node $ foldl insertChild rest newChildren
                where (Just child, rest) = extractChild entries (lhv newEntry)
                      newChildren = map makeEntry . fixNode . hilbertInsert (childTree child) $ newEntry
hilbertInsert (Leaf entries) newEntry = Leaf $ insertChild entries newEntry

extractChild :: [Entry] -> HilbertValue -> (Maybe Entry, [Entry])
extractChild [] _ = (Nothing, [])
extractChild entries hv = (Just (snd child), deleteIndex (fst child) entries)
    where child                = fromMaybe (last numberedEntries) $ find (\e -> hv <= lhv (snd e)) numberedEntries
          numberedEntries      = zip [0..] entries


insertChild :: [Entry] -> Entry -> [Entry]
insertChild entries entry = insertBy (comparing lhv) entry entries



--TODO: complete
-- fixEntries :: HRT -> [Entries] -> [Entries]
-- fixEntries 

fixNode :: HRT -> [HRT]
fixNode tree@(getEntries -> entries)
        | length entries <= maxEntries = [tree]
        | otherwise                    = RTree.split tree

extractSiblings :: Int -> [Entry] -> HilbertValue -> ([Entry], [Entry])
extractSiblings num entries hv = iterate extractAnother ([], entries) !! num
    where extractAnother (siblings, entries) = case extractChild entries hv of
                                                (newSibling, remainingEntries) -> (maybeToList newSibling ++ siblings, remainingEntries)

--TODO: complete
-- rebalance :: [HRT] -> [HRT]
-- rebalance trees = makeTrees concatMap getEntries trees
--     where makeTrees 


split :: HRT -> [HRT]
split tree = [firstTree, secondTree]
    where (firstTree, secondTree)       = (rewrap tree firstEntries, rewrap tree secondEntries)
          (firstEntries, secondEntries) = splitAt halfElems entries
          halfElems                     = length entries `div` 2
          entries                       = getEntries tree



makeEntry :: HRT -> Entry
makeEntry node@(Node entries) = Entry (findMBR $ map mbr entries) (findLHV $ map lhv entries) node
makeEntry leaf@(Leaf entries) = Entry (findMBR $ map mbr entries) (findLHV $ map lhv entries) leaf

findMBR :: [Rect] -> Rect
findMBR rects = Rect lowestX highestX lowestY highestY
    where lowestX  = minimum $ map lowX rects
          highestX = maximum $ map highX rects
          lowestY  = minimum $ map lowY rects
          highestY = maximum $ map highY rects
findLHV :: [HilbertValue] -> HilbertValue
findLHV = maximum 

intersects :: Rect -> Rect -> Bool
r1 `intersects` r2 =    lowX r1 < highX r2 && lowX r2 < highX r1
                     && lowY r1 < highY r2 && lowY r2 < highY r1
