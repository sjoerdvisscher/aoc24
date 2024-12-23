import qualified Data.Map as M
import qualified Data.Set as S
import Data.Containers.ListUtils (nubOrd)
import Data.List.Split (splitOn)
import Data.List (sortOn, intercalate, sort)
import Data.Algorithm.MaximalCliques

main :: IO ()
main = interact (part2 . parse . lines)

parse :: [String] -> M.Map String [String]
parse ls = M.fromListWith (++) (ls >>= f . splitOn "-")
  where f [a,b] = [(a, [b]), (b, [a])]

part1 :: M.Map String [String] -> String
part1 m = show $ length $ nubOrd $ [ S.fromList [a, b, c] | a <- M.keys m, a !! 0 == 't', b <- m M.! a, c <- m M.! a, b < c, b `elem` m M.! c ]

part2 :: M.Map String [String] -> String
part2 m = intercalate "," $ sort $ last $ sortOn length $ getMaximalCliques (\a b -> b `elem` m M.! a) (M.keys m)
