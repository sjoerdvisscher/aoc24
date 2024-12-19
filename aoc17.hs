import Data.List.Split (splitOn)
import Data.List (find)
import Data.Bits (shiftR, xor)

main :: IO ()
main = interact (val . parse . lines)

parse :: [String] -> ((Int, Int, Int), [Int])
parse ls = ((a, b, c), map read $ splitOn "," $ drop 9 $ ls !! 4)
  where
    [a,b,c] = map (read . drop 12) $ take 3 ls

val :: ((Int, Int, Int), [Int]) -> String
val (_, code) = show (aa, go 0 (aa, 0, 0), code)
  where
    aa = calc (reverse code) 0
    calc [] a = a `div` 8
    calc (x:xs) a0 = calc xs (a * 8)
      where
        Just a = find (\a' -> x == go 0 (a', 0, 0) !! 0) [a0..]
    go :: Int -> (Int, Int, Int) -> [Int]
    go pp (a, b, c)
      | pp < 0 || pp >= length code = []
      | otherwise = run (take 2 $ drop pp code)
      where
        run [0, op] = go (pp + 2) (a `shiftR` combo op, b, c)
        run [1, op] = go (pp + 2) (a, b `xor` op, c)
        run [2, op] = go (pp + 2) (a, combo op `mod` 8, c)
        run [3, op] = go (if a == 0 then pp + 2 else op) (a, b, c)
        run [4, _] = go (pp + 2) (a, b `xor` c, c)
        run [5, op] = (combo op `mod` 8) : go (pp + 2) (a, b, c)
        run [6, op] = go (pp + 2) (a, a `shiftR` combo op, b)
        run [7, op] = go (pp + 2) (a, b, a `shiftR` combo op)
        combo 4 = a
        combo 5 = b
        combo 6 = c
        combo n = n
