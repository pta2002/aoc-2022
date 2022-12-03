import Control.Arrow
import Data.Char
import Data.List.Split
import qualified Data.Set as S

exampleData =
    [ "vJrwpWtwJgWrhcsFMMfFFhFp"
    , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    , "PmmdzqPrVvPwwTWBwg"
    , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    , "ttgJtRGJQctTZtZT"
    , "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

halve :: [a] -> ([a], [a])
halve = uncurry splitAt . ((`div` 2) . length &&& id)

twoSets :: Ord a => ([a], [a]) -> (S.Set a, S.Set a)
twoSets = (S.fromList *** S.fromList)

common :: Ord a => (S.Set a, S.Set a) -> S.Set a
common = uncurry S.intersection

parse :: String -> [String]
parse = lines

priority :: Char -> Int
priority c
    | isLower c = (ord c) - (ord 'a') + 1
    | otherwise = (ord c) - (ord 'A') + 27

part1 :: [String] -> Int
part1 = sum . map (priority . head . S.toList <$> common . twoSets . halve)

intersectionOfList :: Ord a => [S.Set a] -> S.Set a
intersectionOfList [a, b] = S.intersection a b
intersectionOfList (x:xs) = S.intersection x $ intersectionOfList xs

-- commonInGroup :: [String] -> [String]
part2 :: [String] -> Int
part2 = sum . map (priority . head . S.toList . intersectionOfList . map S.fromList) . chunksOf 3

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)
