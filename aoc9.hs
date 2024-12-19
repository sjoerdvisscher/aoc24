main :: IO ()
main = interact (show . val2 . map (read . pure))

val1 :: [Int] -> Int
val1 inp = filled inp 0 filler [0..] 0
  where
    files = zip [0..] $ go inp where go [] = []; go [x] = [x]; go (x:_:xs) = x : go xs
    filler = reverse files >>= \(i, cnt) -> replicate cnt i
    filled (c:cs) i fs bs chk
      | i == fs !! 0 = chk + sum (zipWith (*) (takeWhile (==i) fs) bs)
      | otherwise = unfilled cs i fs (drop c bs) (chk + i * sum (take c bs))
    filled [] _ _ _ chk = chk
    unfilled (c:cs) i fs bs chk
      | any (== i) filling = chk + sum (zipWith (*) (takeWhile (>i) filling) bs)
      | otherwise = filled cs (i + 1) (drop c fs) (drop c bs) (chk + sum (zipWith (*) filling (take c bs)))
      where
        filling = take c fs
    unfilled [] _ _ _ chk = chk

val2 :: [Int] -> Int
val2 inp = go files holes 0
  where
    go [] _ chk = chk
    go (f:fs) hs chk = case fill f hs of (hs', chk') -> go fs hs' (chk + chk')
    fill (i, (s, b)) [] = ([], i * sum (take s [b..]))
    fill f@(i, (s, b)) ((h, c):hs)
      | h >= s && c < b = ((h - s, c + s): hs, i * sum (take s [c..]))
      | otherwise = case fill f hs of (hs', chk) -> ((h, c):hs', chk)
    inpWithPos = zip inp (scanl (+) 0 inp)
    (files', holes) = go inpWithPos
      where
        go [] = ([], [])
        go [x] = ([x], [])
        go (x:y:xs) = let (fs, hs) = go xs in (x:fs, y:hs)
    files = reverse $ zip [0..] files'