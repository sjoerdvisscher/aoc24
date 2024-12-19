main :: IO ()
main = interact (show . val . lines)

val :: [String] -> Int
val lns' = sum [ (go (x - 1) (y - 1) 1 1 "MAS" + go (x + 1) (y + 1) (-1) (-1) "MAS")
               * (go (x - 1) (y + 1) 1 (-1) "MAS" + go (x + 1) (y - 1) (-1) 1 "MAS")
               | x <- [3..h+3], y <- [3..w+3] ]
  where
    go _ _ _ _ "" = 1
    go x y dx dy (c:cs) = if lns !! x !! y == c then go (x + dx) (y + dy) dx dy cs else 0
    w = length (lns' !! 0)
    h = length lns'
    lns = replicate 3 (replicate (w + 6) '.') ++ map (\l -> "..." ++ l ++ "...") lns' ++ replicate 3 (replicate (w + 6) '.')