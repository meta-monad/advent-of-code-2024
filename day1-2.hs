import System.IO
import Data.List
import Data.Maybe
import Control.Monad

makeTuples :: [String] -> Maybe (Int, Int)
makeTuples (x:(x':_)) = Just (read x, read x')
makeTuples _ = Nothing

main :: IO ()
main = do
    handle <- openFile "day1-1.txt" ReadMode
    contents <- map (fromJust . makeTuples . words) . lines <$> hGetContents handle
    print $ sum $ map (\key -> key * length ( filter (==key) $ map snd contents )) (nub $ map fst contents)
    hClose handle
    return ()
