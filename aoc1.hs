import Data.Bifunctor (bimap)
import Data.Map (Map, fromListWith, toList, findWithDefault)

main :: IO ()
main = interact (show . sum . calc . bimap mkMap mkMap . unzip . map val . lines)

val :: String -> (Integer, Integer)
val s = case words s of [s1, s2] -> (read s1, read s2)

mkMap :: [Integer] -> Map Integer Integer
mkMap = fromListWith (+) . map (, 1)

calc :: (Map Integer Integer, Map Integer Integer) -> [Integer]
calc (m1, m2) = map (\(k, v) -> k * v * findWithDefault 0 k m2) $ toList m1