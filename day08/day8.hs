{-# LANGUAGE TupleSections #-}
import Data.Array
import Control.Arrow

exampleData :: Array (Int, Int) Int
exampleData = fromMatrix
    [ [3, 0, 3, 7, 3]
    , [2, 5, 5, 1, 2]
    , [6, 5, 3, 3, 2]
    , [3, 3, 5, 4, 9]
    , [3, 5, 3, 9, 0]
    ]

fromMatrix :: [[Int]] -> Array (Int, Int) Int
fromMatrix = (id &&& concat)
  >>> first (((0,0),) . (pred . length . head &&& pred . length))
  >>> uncurry listArray

visibleSide :: Int -> [Int] -> Bool
visibleSide _ [] = True
visibleSide i list = i > maximum list

visible :: Int -> [[Int]] -> Bool
visible i = any (visibleSide i)

sides :: Array (Int, Int) Int -> (Int, Int) -> [[Int]]
sides arr (x, y) = [a, b, c, d]
  where
    a = reverse . map snd . filter (\((x1, y1), _) -> x1 < x && y1 == y) $ assocs arr
    b = map snd . filter (\((x1, y1), _) -> x1 > x && y1 == y) $ assocs arr
    c = reverse . map snd . filter (\((x1, y1), _) -> y1 < y && x1 == x) $ assocs arr
    d = map snd . filter (\((x1, y1), _) -> y1 > y && x1 == x) $ assocs arr

isVisible :: Array (Int, Int) Int -> ((Int, Int), Int) -> Bool
isVisible map (pos, v) = visible v $ sides map pos

visibilityMap :: Array (Int, Int) Int -> Array (Int, Int) Bool
visibilityMap m = listArray (bounds m) $ isVisible m <$> assocs m

part1 :: Array (Int, Int) Int -> Int
part1 = length . filter id . elems . visibilityMap

parse :: String -> Array (Int, Int) Int
parse = fromMatrix . map (map (read . (:[]))) . lines

score :: Array (Int, Int) Int -> (Int, Int) -> Int
score m p = product . map visibleOnSide . sides m $ p
  where
    visibleOnSide p = min (length p) (succ . length . takeWhile (< v) $ p)
    v = m ! p

part2 :: Array (Int, Int) Int -> Int
part2 m = maximum $ map (score m) $ indices m

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
