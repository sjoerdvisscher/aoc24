
import qualified Data.Map as M
import Linear hiding (trace)
import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main = interact (show . part2 . map parse . lines)

w, h :: Int
w = 101
h = 103

type Robot = (V2 Int, V2 Int)

parse :: String -> Robot
parse s = (V2 px py, V2 vx vy)
  where
    [[px, py], [vx, vy]] = map (map read . splitOn ",") $ splitOn " v=" $ drop 2 s

move :: Int -> Robot -> Robot
move t (p, v) = let V2 x y = p + t *^ v in (V2 (x `mod` w) (y `mod` h), v)

part1 :: [Robot] -> Int
part1 rs = product [ m M.! (x, y) | x <- [LT, GT], y <- [LT, GT] ]
  where
    m = M.fromListWith (+) $ map (\r -> let (V2 x y, _) = move 100 r in ((compare x (w `div` 2), compare y (h `div` 2)), 1)) rs

part2 :: [Robot] -> Int
part2 rs = debug $ (!! 0) $ filter (\(_, rs) -> centeredX rs && centeredY rs) $ test
  where
    test = zip [0..] $ iterate (map (move 1)) rs
    centeredX rs = length (filter (\(V2 x _, _) -> x > 25 && x < 75) rs) > 300
    centeredY rs = length (filter (\(V2 _ y, _) -> y > 25 && y < 75) rs) > 300
    debug (i, rs) = trace s i
      where
        s = show i ++ "\n" ++ unlines [ [ if (V2 x y) `elem` map fst rs then '#' else '.' | x <- [0..w-1] ] | y <- [0..h-1] ] ++ "\n"
