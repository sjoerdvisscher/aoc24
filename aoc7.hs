import Data.Foldable (foldlM)
import Control.Parallel.Strategies

main :: IO ()
main = interact (show . sum . withStrategy (parTraversable rseq) . map (val . parse) . lines)

parse :: String -> (Int, [Int])
parse s = let h:t = words s in (read (init h), map read t)

val :: (Int, [Int]) -> Int
val (i, (x:xs)) = if any (== i) opts then i else 0
  where
    opts = foldlM (\a b -> [a + b, a * b, read (show a ++ show b)]) x xs