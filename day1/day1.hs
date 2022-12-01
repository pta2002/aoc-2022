import Control.Applicative
import Control.Monad
import qualified Data.List as L
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Data.Either

type CalorieList = [[Int]]

exampleData :: CalorieList
exampleData =
    [ [1000, 2000, 3000]
    , [4000]
    , [5000, 6000]
    , [7000, 8000, 9000]
    , [10000]
    ]

parseList :: GenParser Char st CalorieList
parseList = sepBy group newline

group :: GenParser Char st [Int]
group = sepEndBy1 integer newline

integer :: GenParser Char st Int
integer = read <$> many1 digit

countCalories :: CalorieList -> [Int]
countCalories = map sum

top3Calories :: [Int] -> [Int]
top3Calories = take 3 . L.sortBy (flip compare)

part1 :: CalorieList -> Int
part1 = maximum . countCalories

part2 :: CalorieList -> Int
part2 = sum . top3Calories . countCalories

rightOrError :: Show a => Either a b -> b
rightOrError (Right b) = b
rightOrError (Left a) = error $ show a

main :: IO ()
main = do
    list <- rightOrError <$> parseFromFile parseList "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 list)
    putStrLn $ "Part 2: " ++ (show $ part2 list)
