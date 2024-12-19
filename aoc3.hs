import Text.Regex.TDFA

main :: IO ()
main = interact (show . val)

re :: String
re = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"

val :: String -> Int
val inp = go True 0 muls
  where
    muls = getAllTextMatches (inp =~ re)
    mul s = product $ map read (tail $ getAllTextSubmatches (s =~ re))
    go _ acc [] = acc
    go _ acc ("do()":xs) = go True acc xs
    go _ acc ("don't()":xs) = go False acc xs
    go False acc (_:xs) = go False acc xs
    go True acc (m:xs) = go True (acc + mul m) xs