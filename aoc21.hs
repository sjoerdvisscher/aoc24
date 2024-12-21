import qualified Data.Map as M
import Control.Monad (replicateM)

main :: IO ()
main = interact (show . sum . map val . lines)

topLevel :: Int
topLevel = 25

val :: String -> Int
val s = read (init s) * typ 0 s

typ :: Int -> String -> Int
typ level s = go (m M.! 'A') s
  where
    pad = if level == 0 then ["789", "456", "123", " 0A"] else [" ^A", "<v>"]
    m = M.fromList $ [ (c, (x, y)) | (y, row) <- zip [0..] pad, (x, c) <- zip [0..] row ]
    go _ [] = 0
    go (x, y) (c:cs) = best + go (x', y') cs
      where
        (x', y') = m M.! c
        repX = replicate (abs (x - x'))
        repY = replicate (abs (y - y'))
        forbidden = if level == 0
          then if (0, 3) == (min x x', max y y') then
            if x' > x then [repY 'v' ++ repX '>'] else [repX '<' ++ repY '^'] else []
          else if (0, 0) == (min x x', min y y') then
            if x' > x then [repY '^' ++ repX '>'] else [repX '<' ++ repY 'v'] else []
        best = minimum
          [ typCached (level + 1) (keys ++ "A")
          | keys <- mix (repX (if x' > x then '>' else '<')) (repY (if y' > y then 'v' else '^'))
          , keys `notElem` forbidden ]

typCached :: Int -> String -> Int
typCached l s | l > topLevel = length s
typCached l s = cache M.! (l, s)

cache :: M.Map (Int, [Char]) Int
cache = M.fromList [ ((level, keys), typ level keys) | level <- [1..topLevel], len <- [0..5], keys' <- replicateM len "<>^v", let keys = keys' ++ "A" ]

mix :: [a] -> [a] -> [[a]]
mix [] ys = [ys]
mix xs [] = [xs]
mix (x:xs) (y:ys) = ((x :) <$> mix xs (y:ys)) ++ ((y :) <$> mix (x:xs) ys)