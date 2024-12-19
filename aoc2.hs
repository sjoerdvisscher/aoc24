
main :: IO ()
main = interact (show . sum . map (fromEnum . val . map read . words) . lines)

val :: [Int] -> Bool
val l = any (\opt -> all (\x -> x >= 1 && x <= 3) opt || all (\x -> x >= -3 && x <= -1) opt) diffs
  where
    diffs = map (\xs -> zipWith (-) (tail xs) xs) opts
    opts = l : drops l

drops :: [a] -> [[a]]
drops [] = []
drops (x:xs) = xs : map (x:) (drops xs)