import Control.Arrow
import Data.Char
import Data.List (transpose)
import Data.List.Split
import Data.Maybe
import Data.Array

type Tower = [Char]
type Move = (Int, Int, Int) -- N, From, To

exampleData :: [Tower]
exampleData =
    [ "NZ"
    , "DCN"
    , "P"
    ]

parseStacks :: String -> Array Int Tower
parseStacks = listArray (1, 9) . map catMaybes . transpose . map parseStackLine . lines

parseStackLine :: String -> [Maybe Char]
parseStackLine = map (listToMaybe . filter isAlpha) . chunksOf 4

first2 :: [a] -> (a, a)
first2 (a : b : _) = (a, b)

splitInput :: String -> (String, String)
splitInput = first2 . splitOn "\n\n"

parseMove :: String -> Move
parseMove move = (read n, (read f), (read t))
  where
    [_, n, _, f, _, t] = splitOn " " move

parse :: String -> (Array Int Tower, [Move])
parse = (parseStacks *** (map parseMove . lines)) . splitInput

execute :: Move -> Array Int Tower -> Array Int Tower
execute (n, from, to) stacks = stacks // [(from, newStackFrom), (to, newStackTo)]
  where
    oldStackFrom = stacks ! from
    oldStackTo = stacks ! to
    newStackFrom = drop n oldStackFrom
    newStackTo = (reverse $ take n oldStackFrom) ++ oldStackTo

-- part1 :: Array Int Tower -> [Move] -> Array Int Char
part1 s = fmap id . foldr execute s

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ uncurry part1 input)
