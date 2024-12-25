import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Bits (shiftL)

main :: IO ()
main = interact (part2 . parse)

parse :: String -> (M.Map String Bool, [[String]])
parse inp = (valMap, map words $ lines fmls)
  where
  [vals, fmls] = splitOn "\n\n" inp
  valMap = M.fromList $ map (\l -> case splitOn ": " l of [n, v] -> (n, v == "1")) $ lines vals

part1 :: (M.Map String Bool, [[String]]) -> Int
part1 (vals, fmls) = go 0 0
  where
    go n acc = maybe acc (\b -> go (n+1) (acc + (fromEnum b `shiftL` n))) $ M.lookup (z n) m
    m = vals <> (M.fromList $ map (\[l, o, r, _, tgt] -> (tgt, op o (m M.! l) (m M.! r))) fmls)
    op "AND" = (&&)
    op "OR" = (||)
    op "XOR" = (/=)

sh :: Int -> String
sh n = if n < 10 then "0" ++ show n else show n

x, y, z :: Int -> String
x n = "x" ++ sh n
y n = "y" ++ sh n
z n = "z" ++ sh n

part2 :: (M.Map String Bool, [[String]]) -> String
part2 (vals, fmls) = show wrongs
  where
    zeros = False <$ vals
    wrongs = concatMap (\n -> if part1 (M.insert (x n) True $ M.insert (y n) True $ zeros, fmls) /= 2 `shiftL` n then [n] else []) [0..44]

-- dmh, z31
-- z11, rpv
-- ctg, rpb
-- dvq, z38