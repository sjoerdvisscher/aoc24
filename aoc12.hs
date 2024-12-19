import UnionFind
import Control.Monad.ST
import Data.Foldable (for_)
import Control.Monad (when)

main :: IO ()
main = interact (show . val . lines)

val :: [[Char]] -> Int
val inp = sum $ map (\(s, fns) -> s * sum (map count fns)) $ runST $ do
  uf <- newWith (w * h) (plots0 !!)
  for_ [(n, s) | n <- [0..w*h-1], s <- sides] (go uf)
  allAnns uf
  where
    sides = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    ps = [(x, y) | x <- [1..w], y <- [1..h]]
    ix (x, y) = y - 1 + (x - 1) * h
    plots0 = [[[(ix p, s) | s <- sides]] | p <- ps]
    go :: UnionFind s [[(Int, (Int, Int))]] -> (Int, (Int, Int)) -> ST s ()
    go uf (i, s@(dx, dy)) =
      let (x0, y0) = ps !! i
          (x1, y1) = (x0 + dx, y0 + dy)
          j = ix (x1, y1)
      in when (m !! y0 !! x0 == m !! y1 !! x1) $ do
        conn <- connected uf i j
        if conn
          then updateAnn uf i (disconnect (i, s) (j, op s))
          else union uf i j (connect (i, s) (j, op s))
    connect _ _ [] r = r
    connect _ _ l [] = l
    connect p0 p1 (l:ls) rs | p0 `notElem` l = l : connect p0 p1 ls rs
    connect p0 p1 ls (r:rs) | p1 `notElem` r = r : connect p0 p1 ls rs
    connect p0 p1 (l:ls) (r:rs) = (between p0 p0 l ++ between p1 p1 r) : (ls ++ rs)
    disconnect _ _ [] = []
    disconnect p0 p1 (fns:fnss)
      | (p0 `elem` fns) && (p1 `elem` fns) =
        filter (not . null) [ between p1 p0 fns, between p0 p1 fns ] ++ fnss
      | otherwise = fns : disconnect p0 p1 fnss
    count fns = sum $ zipWith (\d1 d2 -> if snd d1 == snd d2 then 0 else 1) fns (drop 1 (cycle fns))
    op (dx, dy) = (-dx, -dy)
    between p0 p1 = takeWhile (/= p1) . drop 1 . dropWhile (/= p0) . cycle
    w = length (inp !! 0)
    h = length inp
    m = [replicate (w + 2) '.'] ++ map (\l -> "." ++ l ++ ".") inp ++ [replicate (w + 2) '.']
