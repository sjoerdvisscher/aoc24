import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Bits (shiftL)
import Data.List (intercalate, sort)
import Control.DeepSeq
import Control.Exception
import GHC.IO (unsafePerformIO)

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

part1' :: (M.Map String Bool, [[String]]) -> Maybe Int
part1' inp = unsafePerformIO $ handle (\NonTermination -> pure Nothing) $ pure $!! Just (part1 inp)

sh :: Int -> String
sh n = if n < 10 then "0" ++ show n else show n

x, y, z :: Int -> String
x n = "x" ++ sh n
y n = "y" ++ sh n
z n = "z" ++ sh n

part2 :: (M.Map String Bool, [[String]]) -> String
part2 (vals, fmls) = intercalate "," (sort swaps)
  where
    deps = M.fromList $ map (\[l, _, r, _, tgt] -> (tgt, [l, r])) fmls
    levels = go 0 S.empty
      where
        go 45 _ = []
        go n prevs = level : go (n+1) (prevs <> level)
          where
            level = go' [z n] S.empty
            go' [] acc = acc
            go' (tgt:rest) acc = go' (rest ++ nexts) (S.insert tgt acc)
              where nexts = filter ((`notElem` "xy") . (!! 0)) $ filter (not . (`S.member` prevs)) $ deps M.! tgt
    zeros = False <$ vals
    isWrong fmls' n = part1' (M.insert (x n) True $ zeros, fmls') /= Just (1 `shiftL` n)
    check fmls' n = (part1' (M.insert (x n) True $ M.insert (x (n + 1)) True $ M.insert (y n) True $ zeros, fmls') == Just (4 `shiftL` n)) && not (isWrong fmls' n)
    swaps = concat [ [a, b] | n <- [0..43], isWrong fmls n, let opts = S.toList $ (levels !! n) <> (levels !! (n+1)), a <- opts, b <- opts, a < b, let fmls' = swap a b, check fmls' (n - 1), check fmls' n, check fmls' (n + 1) ]
    swap a b = map (\[l, o, r, _, tgt] -> [l, o, r, "", if tgt == a then b else if tgt == b then a else tgt]) fmls
