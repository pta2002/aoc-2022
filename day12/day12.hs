{-# LANGUAGE TupleSections #-}
import Data.Char (ord)
import qualified Data.Set as S
import Control.Arrow
import Data.Array
import Data.List (find)
import Data.Maybe
import qualified Data.Heap as H
import Algorithm.Search (dijkstra)
import Debug.Trace

type Pos = (Int, Int)

exampleData = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

fromMatrix :: [[a]] -> Array Pos a
fromMatrix = (id &&& concat)
  >>> first (((0, 0),) . (pred . length &&& pred . length . head))
  >>> uncurry listArray

parse :: String -> (Pos, Pos, Array Pos Int)
parse s = (start, end, m')
  where
    m = fromMatrix $ lines s
    start = fst . fromMaybe undefined . find ((== 'S') . snd) . assocs $ m
    end = fst . fromMaybe undefined . find ((== 'E') . snd) . assocs $ m
    m' = toN <$> m
    toN c
      | c == 'S' = 0
      | c == 'E' = 25
      | otherwise = (ord c) - 97

adjacent :: Pos -> S.Set Pos
adjacent (x, y) = S.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

possibleMoves :: Array Pos Int -> Pos -> S.Set Pos
possibleMoves m p = S.filter ((<= v) . (m !)) . S.filter (inRange b) $ adjacent p
  where
    v = (m ! p) + 1
    b = bounds m

possibleMoves2 :: Array Pos Int -> Pos -> S.Set Pos
possibleMoves2 m p = S.filter ((>= v) . (m !)) . S.filter (inRange b) $ adjacent p
  where
    v = (m ! p) - 1
    b = bounds m

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe (Just a) = a

findPath :: Array Pos Int -> Pos -> Pos -> Maybe (Int, [Pos])
findPath m s e = dijkstra (possibleMoves m) (const . const 1) (== e) (s)

findPath2 :: Array Pos Int -> Pos -> Maybe (Int, [Pos])
findPath2 m e = dijkstra (possibleMoves2 m) (const . const 1) (\p -> (m ! p) == 0) (e)

part1 :: (Pos, Pos, Array Pos Int) -> Int
part1 (start, end, m) = fst . unsafeFromMaybe $ findPath m start end

part2 :: (Pos, Pos, Array Pos Int) -> Int
part2 (_, end, m) = fst . unsafeFromMaybe $ findPath2 m end

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
