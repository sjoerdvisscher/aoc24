import qualified Data.Map as M
import Data.Containers.ListUtils (nubOrd)
import Linear.V2

main :: IO ()
main = interact (show . val . parse . lines)

type Pt = V2 Int

parse :: [[a]] -> M.Map Pt a
parse = M.fromList . concat . zipWith (\y -> zipWith (\x c -> (V2 x y, c)) [0..]) [0..]

val :: M.Map Pt Char -> Int
val m = length $ nubOrd
  [ p
  | (p1, c1) <- M.assocs m
  , (p2, c2) <- M.assocs m
  , c1 /= '.'
  , c2 /= '.'
  , p1 /= p2
  , c1 == c2
  , p <- take 50 $ iterate (+ (p2 - p1)) p2
  , p `M.member` m
  ]
