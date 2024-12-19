import qualified Data.Map as M
import qualified Data.Set as S
import Linear.V2
import Control.Monad (join)

main :: IO ()
main = interact (show . val . parse . lines)

type Pt = V2 Int

parse :: [[Char]] -> (Pt, S.Set Pt)
parse lns = (S.findMin (m M.! '^'), m M.! '#')
  where
    m = M.fromListWith S.union [ (c, S.singleton (V2 x y)) | (y, ln) <- zip [0..] lns, (x, c) <- zip [0..] ln ]

val :: (Pt, S.Set Pt) -> Int
val (p0, crates0) = length $ S.fromList $ part2 thePath
  where
    thePath = path crates0 p0 (V2 0 (-1))
    path crates p@(V2 x y) d@(V2 dx dy)
      | x > 129 || y > 129 || x < 0 || y < 0 = []
      | otherwise = (p, d) : if (p + d) `S.member` crates then path crates p (V2 (-dy) dx) else path crates (p + d) d
    part1 = map fst thePath
    part2 = foldMap (\(p, d) -> if loops (path ((p + d) `S.insert` crates0) p0 (V2 0 (-1))) then [p + d] else [])
    loops = join tortoiseHare
      where
        tortoiseHare (x:xs) (_:y:ys) = x == y || tortoiseHare xs ys
        tortoiseHare _ _ = False