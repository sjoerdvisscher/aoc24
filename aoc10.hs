import qualified Data.Map as M

main :: IO ()
main = interact (show . val . lines)

val :: [[Char]] -> Int
val lns' = sum (map (\z -> go '1' [(z, 1)]) zeros)
  where
    go ':' pts = sum (map snd pts)
    go c pts = go (succ c) $ M.toList $ M.fromListWith (+)
      [ ((x', y'), n)
      | ((x, y), n) <- pts
      , (x', y') <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      , lns !! y' !! x' == c ]
    w = length (lns' !! 0)
    h = length lns'
    lns = [replicate (w + 2) '.'] ++ map (\l -> "." ++ l ++ ".") lns' ++ [replicate (w + 2) '.']
    zeros = [(x, y) | x <- [1..w+1], y <- [1..h+1], lns !! y !! x == '0']