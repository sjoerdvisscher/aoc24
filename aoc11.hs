import qualified Data.Map as M
import Data.Maybe (listToMaybe)

main :: IO ()
main = interact (show . val . map read . words)

val :: [Int] -> Int
val ns = sum $ map (go' 0) ns
  where
    go' i n = M.findWithDefault (go i n) (i, n) m
    m = M.fromList [((i, n), go i n) | i <- [0..75::Int], n <- [0..2024]]
    go 75 _ = 1
    go i 0 = go' (i+1) 1
    go i n = maybe (go' (i+1) (n * 2024)) (sum . map (go' (i+1))) $ split n

split :: Int -> Maybe [Int]
split n = listToMaybe
  [ [a, b]
  | lo <- takeWhile (< n) $ iterate (*10) 1
  , let hi = lo * 10
  , let (a, b) = n `divMod` hi
  , hi > a, a >= lo ]
