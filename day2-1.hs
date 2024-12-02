{-# LANGUAGE ScopedTypeVariables #-}
import System.IO

data Safety = Unsafe | Safe deriving (Eq, Show)

toSafe :: Bool -> Safety
toSafe True = Safe
toSafe _ = Unsafe

fstOf3 :: (a,b,c) -> a
fstOf3 (a,_,_) = a

isSafe :: forall a . (Num a, Ord a) => (Safety, Bool, a) -> a -> (Safety, Bool, a)
isSafe (Unsafe, dir, _) x = (Unsafe, dir, x)
isSafe (Safe, dir, prev) next = if dir
                                then (toSafe $ (1 <= diff False) && (diff False <= 3), True, next)
                                else (toSafe $ (1 <= diff True)  && (diff True  <= 3), False, next)
    where diff :: Bool -> a
          diff True = prev - next
          diff _ = next - prev

main :: IO ()
main = do
    fh <- openFile "day2-1.txt" ReadMode
    contents :: [[Int]] <- map (map read . words) . lines <$> hGetContents fh
    print $ length
          $ filter ((==Safe) . fstOf3)
          $ map (\line -> foldl isSafe (Safe, line !! 0 < line !! 1, head line) $ tail line) contents
