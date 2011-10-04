import System.Environment
import Control.Monad
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map

max_line_length :: Integer
max_line_length = 80 

-- In many functions that process data through a series of steps, 
--  one variable is being passed through a long chain of functions
--  each of which modifies it and passes it along to the next.  I find
--  this process much easier to read when the functions are written/called
--  left-to-right.
($>) :: a -> (a->b) -> b
x $> f = f x
infixl 0 $>

main :: IO()
main = do
 args <- getArgs
 contents <- if null args then getContents else liftM unlines (mapM readFile args)
 putStrLn (contents $> parse_words $> count_words $> make_bar_chart)

make_bar_chart :: Map.Map String Integer -> String
make_bar_chart word_map = sorted_word_list $> map (make_bar padded_length scaling_factor) $> filter (not.null) $> unlines
   where sorted_word_list = List.sortBy (\(_, count1) (_, count2) -> compare count2 count1) (Map.assocs word_map)
         padded_length = maximum (map (length . fst) sorted_word_list) + 1
         max_count = snd $ head sorted_word_list
         scaling_factor = min 1.0 $ (fromIntegral max_line_length - fromIntegral padded_length) / fromIntegral max_count

make_bar :: Int -> Double -> (String, Integer) -> String
make_bar padded_length scaling_factor (word, count) | bar_length == 0 = ""
                                                    | otherwise       = padded_word ++ bar
   where padded_word = word ++ replicate (padded_length - length word) ' '
         bar_length = round $ fromInteger count * scaling_factor
         bar = replicate bar_length '#'

-- strict for efficiency (not properly profiled, but somewhat empirically tested)
count_words :: [String] -> Map.Map String Integer
count_words = List.foldl' (\mp word -> Map.insertWith' (+) word 1 mp) Map.empty

parse_words :: String -> [String]
parse_words contents = contents $> map Char.toLower $> words $> map strip_punctuation $> filter (not.null)

strip_punctuation :: String -> String
strip_punctuation = strip_end_punctuation . strip_beginning_punctuation
        where strip_beginning_punctuation = dropWhile (not . Char.isAlpha)
              strip_end_punctuation = reverse . dropWhile (not . Char.isAlpha) . reverse -- TODO: make more efficient
