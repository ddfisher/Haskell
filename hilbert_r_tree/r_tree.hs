import qualified Data.Vector as V
($>) :: a -> (a->b) -> b
x $> f = f x
infixl 0 $>

node_capacity = 5

data Entry = Entry {mbr :: Rect, lhv :: HilbertValue, child_tree :: HRT}
data HRT = Node [Entry] | Leaf [Rect]

data Rect = Rect { lowX :: Int
            , highX :: Int
            , lowY :: Int
            , highY :: Int }

newtype Point = Point (Int, Int)
newtype HilbertValue = HilbertValue Int deriving (Eq, Ord)

find_center :: Rect -> Point
find_center (Rect lx hx ly hy) = Point ( (lx + hx) `div` 2, (ly+hy) `div` 2 )

find_hilbert_value :: Rect -> HilbertValue
find_hilbert_value = undefined --TODO: complete

intersects :: Rect -> Rect -> Bool
intersects r1 r2 =    lowX r1 < highX r2 && lowX r2 < highX r1
                   && lowY r1 < highY r2 && lowY r2 < highY r1

search :: HRT -> Rect -> [Rect]
search (Leaf rects) query = filter (intersects query) rects
search (Node entries) query = concatMap (`search` query) intersecting_children
        where intersecting_children = map child_tree $ filter (intersects query . mbr) entries

-- searches for the first child entry that has lhv > h
remove_child :: HilbertValue -> [Entry] -> (Entry, [Entry])
remove_child h (last_entry:[]) = (last_entry, [])
remove_child h (entry:entries) | lhv entry > h = (entry, entries)
                             | otherwise     = (\(e, es) -> (e, entry:es)) $ remove_child h entries

insert_child :: Entry -> [Entry] -> [Entry]
insert_child c [] = [c]
insert_child c (entry:entries) | lhv entry > lhv c = c:entry:entries
                               | otherwise         = entry:insert_child c entries

insert :: Rect -> HRT -> HRT
insert new_rect (Node entries) | is_full_leaf next_child =  
                               | otherwise = 
            where (next_child, remaining_children) = remove_child (find_hilbert_value new_rect) entries
                h = find_hilbert_value new_rect
                is_full_leaf Leaf rects | length rects >= node_capacity = True
                is_full_leaf _ = False
insert new_rect (Leaf rects) = Leaf (new_list rects)
          where new_list [] = [new_rect]
                new_list (r:rs) | find_hilbert_value r <= find_hilbert_value new_rect = r:new_list rs
                                | otherwise                                           = new_rect:r:rs
