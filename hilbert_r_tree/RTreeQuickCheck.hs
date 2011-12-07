import Test.QuickCheck
import System.Random
import Data.List
import RTree

instance Arbitrary Rect where
    arbitrary = (\x -> choose (Rect 0 x 0 x, undefined)) 1000

instance Random Rect where
    randomR (Rect lX hX lY hY, _) g1 = (buildRect [x1, y1, x2, y2], g5)
                where (x1, g2) = randomR (lX, hX) g1
                      (x2, g3) = randomR (lX, hX) g2
                      (y1, g4) = randomR (lY, hY) g3
                      (y2, g5) = randomR (lY, hY) g4
    random = randomR (Rect 0 (maxSideLen-1) 0 (maxSideLen-1), undefined)

buildRect [x1, y1, x2, y2] = Rect minX maxX minY maxY 
    where maxX = maximum [x1,x2]
          minX = minimum [x1,x2]
          maxY = maximum [y1,y2]
          minY = minimum [y1,y2]

prop_general_check :: [Rect] -> [Rect] -> Bool
prop_general_check rects queries = all (same_query_result rTree rects) queries
    where rTree = foldl' (flip RTree.insert) empty rects

-- same_query_result :: HRT -> [Rect] -> Rect -> Bool
same_query_result rTree rects query = sameElems treeResults listResults
    where treeResults         = search rTree query
          listResults         = filter (intersects query) rects
          sameElems lst1 lst2 = lst1 \\ lst2 == [] && lst2 \\ lst1 == []

main = quickCheck prop_general_check
