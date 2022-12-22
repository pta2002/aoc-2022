{-# LANGUAGE OverloadedRecordDot #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Either
import Data.List (find)
import Debug.Trace

data MPos = MPos { x :: Int, y :: Int } deriving (Show, Eq)
data Sensor = Sensor { pos :: MPos, beacon :: MPos } deriving (Show, Eq)

exampleData = parseSensors $ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
\Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
\Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
\Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
\Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
\Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
\Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
\Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
\Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
\Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
\Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
\Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
\Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
\Sensor at x=20, y=1: closest beacon is at x=15, y=3"

parseInt :: Parsec Void String Int
parseInt = L.signed space L.decimal

parseSensor :: Parsec Void String Sensor
parseSensor = do
  string "Sensor at x="
  sensorX <- parseInt
  string ", y="
  sensorY <- parseInt
  string ": closest beacon is at x="
  beaconX <- parseInt
  string ", y="
  beaconY <- parseInt

  return Sensor { pos = MPos { x = sensorX, y = sensorY }, beacon = MPos { x = beaconX, y = beaconY }}

distance :: MPos -> MPos -> Int
distance a b = (abs $ b.x - a.x) + (abs $ b.y - a.y)

sensorRangeOnY :: Int -> Sensor -> (Int, Int)
sensorRangeOnY y s = if w > 0 then (s.pos.x - w, s.pos.x + w) else (0, 0)
  where
    y' = abs $ y - s.pos.y
    w = (distance s.pos s.beacon) - y'

sensorRangeOnX :: Int -> Sensor -> (Int, Int)
sensorRangeOnX x s = if w > 0 then (s.pos.x - w, s.pos.x + w) else (0, 0)
  where
    x' = abs $ x - s.pos.x
    w = (distance s.pos s.beacon) - x'

mergeRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
mergeRange [] a = [a]
mergeRange ((a, b):t) (c, d)
  | c > b = (a, b):mergeRange t (c, d)
  | d < a = (c, d):(a, b):t
  | c >= a && d <= b = (a, b):t
  | c <= a && d <= b = (c, b):t
  | otherwise = mergeRange t (min a c, max b d)

sensorRangesOnY :: Int -> [Sensor] -> [(Int, Int)]
sensorRangesOnY y = foldl (\acc s -> mergeRange acc $ sensorRangeOnY y s) []

sensorRangesOnX :: Int -> [Sensor] -> [(Int, Int)]
sensorRangesOnX x = foldl (\acc s -> mergeRange acc $ sensorRangeOnX x s) []

rangeSize :: (Int, Int) -> Int
rangeSize = uncurry $ flip (-)

hasHole :: Int -> [(Int, Int)] -> Bool
hasHole _ (_:_:_) = True
hasHole i [(a, b)]
  | a > 0 || b < i = True
  | otherwise = False

part1 :: Int -> [Sensor] -> Int
part1 y = sum . map rangeSize . sensorRangesOnY y

parseSensors :: String -> [Sensor]
parseSensors = rights . map (runParser parseSensor "input.txt") . lines

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe (Just a) = a

findYWithHole :: Int -> [Sensor] -> Int
findYWithHole n s = unsafeFromMaybe $ find (hasHole n . flip sensorRangesOnY s) [0..]

findHole :: Int -> [(Int, Int)] -> Int
findHole n [(a, b)] = if a > 0 then 0 else b + 1
findHole n [(a, b), (c, d)] = b + 1

part2 :: Int -> [Sensor] -> Int
part2 n s = x * 4000000 + y
  where
    y = findYWithHole n s
    x = findHole n $ sensorRangesOnY y s

main :: IO ()
main = do
  input <- parseSensors <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 2000000 input)
  putStrLn $ "Part 2: " ++ (show $ part2 4000000 input)
