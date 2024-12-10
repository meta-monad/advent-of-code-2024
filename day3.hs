import System.IO
import Data.Char
import Control.Monad
-- import Text.Regex.Base
-- import Text.Regex.Base.Context
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data Code = Mul Int Int | Do | Dont deriving Show

doRegEx = "(do\\(\\))"
dontRegEx = "(don't\\(\\))"
mulRegEx = "mul\\(([0-9]+),([0-9]+)\\)"

codeRegEx :: String
codeRegEx = doRegEx <> "|" <> dontRegEx <> "|" <> mulRegEx

pair :: [a] -> (a,a)
pair l = (head l, last l)

extractMatch :: String -> Code
extractMatch m
    | m =~ mulRegEx = (\(a, b) -> Mul a b) $ pair $ extractMul m
    | m =~ doRegEx = Do
    | otherwise = Dont

extractMul :: String -> [Int]
extractMul m = map read $ (\(_, _, _, e) -> e ) ( m =~ mulRegEx :: (String, String, String, [String]))

interpretCode :: (Int, Bool) -> Code -> (Int, Bool)
interpretCode (sum, _) Do = (sum, True)
interpretCode (sum, _) Dont = (sum, False)
interpretCode (sum, False) _ = (sum, False)
interpretCode (sum, True) (Mul a b) = (sum + (a * b), True)


main :: IO ()
main = do
    fh <- openFile "day3-1.txt" ReadMode
    contents <- hGetContents fh
    let matches = getAllTextMatches (contents =~ codeRegEx) :: [String]
    print $ sum $ map (product . extractMul . (=~ mulRegEx)) matches
    print $ fst $ foldl interpretCode (0, True) $ map extractMatch matches
