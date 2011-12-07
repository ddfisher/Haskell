import Data.List
import Control.Monad
import Control.DeepSeq
import System.Environment
import System.CPUTime
import Text.Printf
import RTree

maxResults = 4

time :: (NFData a, Num b) => IO a -> IO (a, b)
time dat = do
    startTime <- getCPUTime
    result <- dat
    result `deepseq` return ()
    endTime <- getCPUTime
    let diff = fromIntegral (endTime - startTime)
    return (result, diff)

main = do
    [filename] <- getArgs
    (rTree, picosecs) <- time $ do
        contents <- readFile filename
        return $ foldl' (flip RTree.insert) empty $ map read (lines contents)
    printf "%s: %d rectangles read in %0.1f milliseconds\n" filename (size rTree) (picosecs/10^9 :: Double)
    queries <- liftM (map read . lines) getContents
    forM_ queries $ \query -> do
        (rects, queryPicos) <- time (return $ search rTree query)
        printf "found %d matches in %0.1f microseconds:\n" (length rects) (queryPicos/10^6 :: Double)
        putStr $ unlines $ map show $ take maxResults rects
