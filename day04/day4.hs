import Data.List.Split

type Range = (Int, Int)

intersection :: Range -> Range -> Range
intersection (a, b) (x, y) = (max a x, min b y)

fullyContained a b = i == a || i == b
    where i = intersection a b

haveOverlap a = uncurry (<=) . intersection a

parseLine :: String -> (Range, Range)
parseLine = first2 . map (first2 . map read . splitOn "-") . splitOn ","

parse = map parseLine . lines

first2 :: [a] -> (a, a)
first2 (a:b:_) = (a, b)

part1 = sum . map (fromEnum . uncurry fullyContained)
part2 = sum . map (fromEnum . uncurry haveOverlap)

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
