import Data.List.Split (splitOn)
import Linear

main :: IO ()
main = interact (show . sum . map (val . parse) . splitOn "\n\n")

parse :: String -> (V2 Double, V2 Double, V2 Double)
parse s = (p "Button A: X+" ", Y+" sa, p "Button B: X+" ", Y+" sb, p "Prize: X=" ", Y=" sp + V2 10000000000000 10000000000000)
  where
    [sa, sb, sp] = lines s
    p pref inf s = let [x, y] = map read $ splitOn inf $ drop (length pref) s in V2 x y

val :: (V2 Double, V2 Double, V2 Double) -> Int
val (va@(V2 ax ay), vb@(V2 bx by), vp@(V2 px py)) = if fromIntegral a *^ va + fromIntegral b *^ vb == vp then a*3 + b else 0
  where
    b = round ((py - px * ay / ax) / (by - bx * ay / ax ))
    a = round ((px - fromIntegral b * bx) / ax)

