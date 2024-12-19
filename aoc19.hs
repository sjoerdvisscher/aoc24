import qualified Data.Map as M
import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . val . parse . lines)

parse :: [String] -> ([String], [String])
parse (opts:_:designs) = (splitOn ", " opts, designs)

newtype Trie = Trie (Int, M.Map Char Trie)

find :: String -> Trie -> Int
find "" (Trie (n, _)) = n
find (c:cs) (Trie (_, m)) = find cs (m M.! c)

val :: ([String], [String]) -> Int
val (opts, designs) = sum $ map possibilitiesCached designs
  where
    possibilities :: [String] -> String -> Int
    possibilities _ "" = 1
    possibilities [] _ = 0
    possibilities (o:os) design = let (d, ds) = splitAt (length o) design in
      (if o == d then possibilitiesCached ds else 0) + possibilities os design
    possibilitiesCached :: String -> Int
    possibilitiesCached design = find design cache
      where
        cache = mkCache ""
        mkCache s = Trie (possibilities opts s, M.fromList [ (c, mkCache (s ++ [c])) | c <- "rgbuw" ])

