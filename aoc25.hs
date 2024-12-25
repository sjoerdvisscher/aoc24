import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = interact (show . val . foldMap parse . splitOn "\n\n")

type Key = [Int]
type Lock = [Int]
parse :: String -> ([Lock], [Key])
parse s = if c == '#' then ([key], []) else ([], [map (5 -) key])
  where
    c = s !! 0
    key = map (pred . length . takeWhile (== c)) $ transpose $ lines s

val :: ([Lock], [Key]) -> Int
val (locks, keys) = sum [ 1 | key <- keys, lock <- locks, all (<= 5) $ zipWith (+) key lock ]