import qualified Data.HashSet as S
import Data.Graph.AStar (aStar)
import Data.List (findIndex, elemIndex)
import Data.Maybe (fromMaybe, fromJust)

main :: IO ()
main = interact (show . val . lines)

val :: [[Char]] -> Int
val m = length $ [ () | (i0, (x0, y0)) <- path, (i1, (x1, y1)) <- drop (i0 + 100) path, let l = abs (x1 - x0) + abs (y1 - y0), l <= 20, i1 - i0 - l >= 100]
  where
    path = zip [0..] $ ((sx,sy):) $ fromJust $ aStar neighbors (\_ _ -> 1) (\_ -> 0) (\(x, y) -> m !! y !! x == 'E') (sx, sy)
    sy = fromMaybe (-1) $ findIndex (elem 'S') m
    sx = fromMaybe (-1) $ elemIndex 'S' (m !! sy)
    neighbors (x, y) = S.fromList [(x', y') | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)],
      let x' = x + dx, let y' = y + dy, m !! y' !! x' /= '#']

