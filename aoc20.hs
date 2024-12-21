import qualified Data.HashSet as S
import Data.Graph.AStar (aStar)
import Data.Maybe (fromJust)

main :: IO ()
main = interact (show . val . lines)

val :: [[Char]] -> Int
val m = length $ [ () | (i0, (x0, y0)) <- path, (i1, (x1, y1)) <- drop (i0 + 100) path, let d = abs (x1 - x0) + abs (y1 - y0), d <= 20, i1 - i0 - d >= 100]
  where
    path = zip [0..] $ (start:) $ fromJust $ aStar neighbors (\_ _ -> 1::Int) (\_ -> 0) (\(x, y) -> m !! y !! x == 'E') start
    start = [ (x, y) | (y, row) <- zip [0..] m, (x, c) <- zip [0..] row, c == 'S'] !! 0
    neighbors (x, y) = S.fromList [(x', y') | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)],
      let x' = x + dx, let y' = y + dy, m !! y' !! x' /= '#']
