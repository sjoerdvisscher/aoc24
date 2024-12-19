import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)
import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . val . parse)

parse :: String -> (M.Map Int (S.Set Int), [[Int]])
parse inp = (M.fromListWith S.union [(k, S.singleton v) | [k, v] <- ords], seqs)
  where
    [inp1, inp2] = splitOn "\n\n" inp
    ords = map read . splitOn "|" <$> lines inp1
    seqs = map read . splitOn "," <$> lines inp2

val :: (M.Map Int (S.Set Int), [[Int]]) -> Int
val (m, ss) = sum [if sorted /= s then middle sorted else 0 | s <- ss, let sorted = sortBy by s ]
  where
    middle s = s !! (length s `div` 2)
    by x y = if S.member y (M.findWithDefault S.empty x m) then LT else GT