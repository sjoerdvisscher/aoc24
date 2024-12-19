import qualified Data.HashSet as S
import Data.List.Split (splitOn)
import Data.Graph.AStar (aStar)
import Debug.Trace

main :: IO ()
main = interact (show . val . parse . lines)

type Pt = (Int, Int)

parse :: [String] -> [Pt]
parse = map (toPt . map read . splitOn ",")
  where toPt [x, y] = (x, y)

size = 70

val :: [Pt] -> Pt
val walls = go 1024
  where
    go n = traceShow n $ case (path $ S.fromList $ take n walls) of
      Just p -> go $ (+(n+1)) $ length $ takeWhile (not . (`S.member` p)) $ drop n walls
      Nothing -> walls !! (n - 1)

path :: S.HashSet Pt -> Maybe (S.HashSet Pt)
path walls = S.fromList <$> aStar neighbors (\_ _ -> 1) (\(x, y) -> size - x + size - y) isEnd start
  where
    start = ((0, 0))
    isEnd (x, y) = x == size && y == size
    neighbors (x, y) = S.fromList [(x', y') | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)],
      let x' = x + dx, let y' = y + dy, x' >= 0 && x' <= size && y' >= 0 && y' <= size, not $ (x', y') `S.member` walls]
