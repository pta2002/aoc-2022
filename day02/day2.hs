import Control.Arrow

data ElfPlays = Rock | Paper | Scissors deriving (Show, Eq)
data MyPlays = X | Y | Z deriving (Show, Eq)

exampleData =
    [ (Rock, Y)
    , (Paper, X)
    , (Scissors, Z)
    ]

outcome :: ElfPlays -> MyPlays -> Int
outcome Rock X = 3
outcome Rock Y = 6
outcome Rock Z = 0
outcome Paper X = 0
outcome Paper Y = 3
outcome Paper Z = 6
outcome Scissors X = 6
outcome Scissors Y = 0
outcome Scissors Z = 3

shapeScore :: ElfPlays -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

convert1 :: MyPlays -> ElfPlays
convert1 X = Rock
convert1 Y = Paper
convert1 Z = Scissors

score :: (ElfPlays, MyPlays) -> Int
score = uncurry (+) . (shapeScore . convert1 . snd &&& uncurry outcome)

part1 :: [(ElfPlays, MyPlays)] -> Int
part1 = sum . map score

outcome2 :: MyPlays -> Int
outcome2 X = 0
outcome2 Y = 3
outcome2 Z = 6

findShape :: ElfPlays -> MyPlays -> ElfPlays
findShape a Y = a
findShape Rock X = Scissors
findShape Rock Z = Paper
findShape Paper X = Rock
findShape Paper Z = Scissors
findShape Scissors X = Paper
findShape Scissors Z = Rock

score2 :: (ElfPlays, MyPlays) -> Int
score2 = uncurry (+) . (shapeScore . uncurry findShape &&& outcome2 . snd)

part2 :: [(ElfPlays, MyPlays)] -> Int
part2 = sum . map score2

elfPlay :: Char -> ElfPlays
elfPlay 'A' = Rock
elfPlay 'B' = Paper
elfPlay 'C' = Scissors

myPlay :: Char -> MyPlays
myPlay 'X' = X
myPlay 'Y' = Y
myPlay 'Z' = Z

parseLine :: String -> (ElfPlays, MyPlays)
parseLine [e, ' ', m] = (elfPlay e, myPlay m)

parse :: String -> [(ElfPlays, MyPlays)]
parse = map parseLine . lines

main = do
  input <- parse <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
