import Debug.Trace
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.List (find)
import Data.List.Split (splitOn)
import Control.Monad.ST
import Data.Foldable (traverse_)

main :: IO ()
main = interact (show . val . mkArr)

type Pt = (Int, Int)

mkArr :: String -> (Array Pt Char, Pt, String)
mkArr s = (m // [(p0, '.')], p0, concat $ lines moves)
  where
    h = length lns
    w = length (lns !! 0)
    m = listArray ((0, 0), (h - 1, w - 1)) (concat lns)
    Just p0 = fst <$> find ((== '@') . snd) (assocs m)
    [s', moves] = splitOn "\n\n" s
    lns = concatMap (\c -> if c == '@' then "@." else if c == 'O' then "[]" else [c, c]) <$> lines s'

val :: (Array Pt Char, Pt, String) -> Int
val (m0, p0, moves) = runST $ do
  mm <- thaw m0
  go mm p0 moves
  score <$> freeze mm
  where
    score = sum . map (\((y, x), c) -> if c == '[' then y * 100 + x else 0) . assocs
    go _ _ [] = pure ()
    go m p (c:cs) = do
      p' <- push (dir c) p m
      go m p' cs
    push (dy, dx) p@(py, px) m = toMove >>= move
      where
        toMove = loop [p]
        move Nothing = pure p
        move (Just (_:crates)) = do
          toWrite <- traverse (\c@(cy, cx) -> ((cy + dy, cx + dx), ) <$> readArray m c) crates
          m & map (, '.') crates
          m & toWrite
          pure (py + dy, px + dx)
        loop ps = do
          maybes <- traverse (\(y, x) -> push' (y + dy, x + dx)) ps
          pure $ (ps ++) . concat <$> sequenceA maybes
        push' p1@(py1, px1) = do
          c <- readArray m p1
          case c of
            '#' -> pure $ Nothing
            '[' -> loop $ if dx == 0 then [p1, (py1, px1 + 1)] else [p1]
            ']' -> loop $ if dx == 0 then [p1, (py1, px1 - 1)] else [p1]
            _ -> pure $ Just []
    dir '^' = (-1, 0)
    dir 'v' = (1, 0)
    dir '<' = (0, -1)
    dir '>' = (0, 1)

(&) :: STArray s Pt Char -> [(Pt, Char)] -> ST s ()
m & cs = traverse_ (uncurry (writeArray m)) cs

dbg :: Array (Int, Int) Char -> a -> a
dbg arr = go (elems arr)
  where
    go [] x = x
    go s x = let (l, s') = splitAt (w + 1) s in trace l $ go s' x
    (_, w) = snd (bounds arr)