import qualified Data.Map as M
import Data.Bits (Bits(..))
import Data.List (tails)
import Control.Parallel.Strategies

main :: IO ()
main = interact (show . val . map (secrets . read) . lines)

secrets :: Int -> [Int]
secrets inp = map (`mod` 10) $ take 2001 $ iterate f inp
  where
    f x = let x' = step x (`shiftL` 8); x'' = x' `xor` (x' `shiftR` 5) in step x'' (`shiftL` 11)
    step x g = (x `xor` g x) .&. 0xFFFFFF

val :: [[Int]] -> Int
val = maximum . M.unionsWith (+) . withStrategy (parTraversable rseq) .
  map (\ss -> M.fromList $ reverse $ zip (take 4 <$> tails (zipWith (-) (drop 1 ss) ss)) (drop 4 ss))
