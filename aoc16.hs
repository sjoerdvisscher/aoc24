import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQ
import Debug.Trace

main :: IO ()
main = interact (show . val . lines)

type Pt = (Int, Int)

val :: [[Char]] -> Int
val m = length $ trace (debug res) res
  where
    res = part2 $ astar (PQ.singleton 0 ((start, Set.empty))) Map.empty
    h = length m
    w = length (m !! 0)
    start = ((1, h - 2), (1, 0))
    isEnd ((x, y), _) = y == 1 && x == w - 2
    neighbors (p@(x, y), (dx, dy)) =
      (if m !! (y + dy) !! (x + dx) /= '#' then [(((x + dx, y + dy), (dx, dy)), 1)] else []) ++
        [((p, (dy, dx)), 1000), ((p, (-dy, -dx)), 1000)]
    part2 :: [(Int, Set.Set Pt)] -> Set.Set Pt
    part2 [] = mempty
    part2 ((c, ps):pss) = ps <> Set.unions (map snd $ takeWhile ((== c) . fst) pss)
    astar :: PQ.MinPQueue Int ((Pt, Pt), Set.Set Pt) -> Map.Map (Pt, Pt) Int -> [(Int, Set.Set Pt)]
    astar PQ.Empty _ = []
    astar ((c, (n, ps)) PQ.:< q) v = (if isEnd n then ((c, ps):) else id) $
      astar (foldl' (\q' (p', c') -> if p' `Map.notMember` v || v Map.! p' >= c + c' then PQ.insert (c + c') (p', Set.insert (fst p') ps) q' else q') q (neighbors n)) v'
      where
        v' = Map.insert n c v

    debug ps = unlines [[if (x, y) `elem` ps then 'O' else m !! y !! x | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
