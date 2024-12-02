import System.IO
import Data.List
import Data.Maybe
import Control.Monad

makeTuples :: [String] -> Maybe (Int, Int)
makeTuples (x:(x':_)) = Just (read x, read x')
makeTuples _ = Nothing

doubleSort :: [(Int, Int)] -> [(Int, Int)]
doubleSort l = zip (sort $ map fst l) (sort $ map snd l)

main :: IO ()
main = do
    handle <- openFile "day1-1.txt" ReadMode
    contents <- map (fromJust . makeTuples . words) . lines <$> hGetContents handle
    print $ sum $ map (\(a,b) -> abs $ a -b ) $ doubleSort contents
    hClose handle
    return ()
