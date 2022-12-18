import Control.Monad.State
import qualified Data.Set as S
import Debug.Trace
import Control.Arrow

type Rope = ((Int, Int), (Int, Int))
type Board = S.Set (Int, Int)
data Direction = L | R | U | D deriving (Eq, Show)

exampleData :: [(Direction, Int)]
exampleData =
    [ (R, 4)
    , (U, 4)
    , (L, 3)
    , (D, 1)
    , (R, 4)
    , (D, 1)
    , (L, 5)
    , (R, 2)
    ]

clamp :: Int -> Int
clamp = max (-1) . min 1

updateRope :: Rope -> Rope
updateRope ((ax, ay), (bx, by)) = if dist >= 2 then ((ax, ay), newT) else ((ax, ay), (bx, by))
  where
    dist = max (abs (bx - ax)) (abs (by - ay))
    chgX = clamp $ ax - bx
    chgY = clamp $ ay - by
    newT = (bx + chgX, by + chgY)

updateChain :: [(Int, Int)] -> [(Int, Int)]
updateChain [a,b] = [a', b']
  where (a', b') = updateRope $! (a, b)
updateChain (a:b:cs) = [a', b'] ++ updateChain (b:cs)
  where (a', b') = updateRope $! (a, b)

move :: (Int, Int) -> [(Int, Int)] -> State Board [(Int, Int)]
move (x, y) ((hx, hy):t) = do
    let r = updateChain ((hx + x, hy + y):t)
    modify $! S.insert $! last r
    return r

dirVector :: Direction -> (Int, Int)
dirVector L = (-1, 0)
dirVector R = (1, 0)
dirVector U = (0, -1)
dirVector D = (0, 1)

moveN :: (Direction, Int) -> [(Int, Int)] -> State Board [(Int, Int)]
moveN (_, 0) r = return r
moveN (dir, n) r = (>>= moveN (dir, n - 1)) $! move (dirVector dir) r 
moves :: [(Direction, Int)] -> [(Int, Int)] -> State Board [(Int, Int)]
moves [] r = return r
moves (m:ms) r = moveN m r >>= moves ms

part1 :: [(Direction, Int)] -> Int
part1 ms = length $ S.toList $ execState (moves ms (take 2 $ repeat (0,0))) S.empty

part2 :: [(Direction, Int)] -> Int
part2 ms = length $ S.toList $ execState (moves ms (take 4 $ repeat (0,0))) S.empty

parseDir :: Char -> Direction
parseDir 'R' = R
parseDir 'L' = L
parseDir 'D' = D
parseDir 'U' = U

parseMove :: String -> (Direction, Int)
parseMove s = (parseDir . head *** read) ws
  where
    [a, b] = words s
    ws = (a, b)

parse :: String -> [(Direction, Int)]
parse = map parseMove . lines

main :: IO ()
main = do
    -- input <- parse <$> readFile "input.txt"
    let input = exampleData
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
