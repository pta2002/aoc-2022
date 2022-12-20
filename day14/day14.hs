import Control.Arrow
import Data.Either
import Data.Ix
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative ((<|>))
import Data.Maybe

data Block = Rock | Sand deriving (Show, Eq)

type MPos = (Int, Int)
type Path = [MPos]
type World = (Int, M.Map MPos Block)

parseInt :: Parsec Void String Int
parseInt = read <$> some digitChar

parsePos :: Parsec Void String MPos
parsePos = do
    a <- parseInt
    single ','
    b <- parseInt

    return (a, b)

parsePath :: Parsec Void String Path
parsePath = parsePos `sepBy` string " -> "

orderRange :: (MPos, MPos) -> (MPos, MPos)
orderRange (a, b)
  | range (a, b) == [] = (b, a)
  | otherwise = (a, b)

blocksBetween :: (MPos, MPos) -> [(MPos, Block)]
blocksBetween = flip zip (repeat Rock) . range . orderRange

slidingWindow :: [a] -> [(a, a)]
slidingWindow [] = []
slidingWindow (_ : []) = []
slidingWindow (a : b : t) = (a, b) : (slidingWindow (b : t))

pathBlocks :: Path -> [(MPos, Block)]
pathBlocks = concat . map blocksBetween . slidingWindow

pathMap :: Path -> M.Map MPos Block
pathMap = M.fromList . pathBlocks

parseWorld :: String -> World
parseWorld =
    runParser (parsePath `sepBy` single '\n') "input.txt"
        >>> fromRight []
        >>> map pathMap
        >>> foldr M.union M.empty
        >>> (calcLowest &&& id)

calcLowest :: M.Map MPos Block -> Int
calcLowest = maximum . map snd . M.keys

exampleData :: World
exampleData =
    parseWorld $
        "498,4 -> 498,6 -> 496,6\n"
            ++ "503,4 -> 502,4 -> 502,9 -> 494,9"

maybeMove :: World -> MPos -> MPos -> Maybe MPos
maybeMove (m, w) (x, y) (a, b) = case w M.!? (x', y') of
  Nothing -> if y' >= m + 2 then Nothing else Just (x', y')
  Just _ -> Nothing
  where
    (x', y') = (x + a, y +b)

moveSand :: World -> MPos -> Maybe MPos
moveSand w a = mvDown <|> mvLeft <|> mvRight
  where
    mvDown = maybeMove w a (0, 1)
    mvLeft = maybeMove w a (-1, 1)
    mvRight = maybeMove w a (1, 1)

iterateMaybe :: (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
iterateMaybe stop f a = case f a of
    Nothing -> Just a
    Just b -> if stop b then Nothing else iterateMaybe stop f b

iterateMaybe2 :: (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
iterateMaybe2 stop f a = case f a of
    Nothing -> if stop a then Nothing else Just a
    Just b -> iterateMaybe2 stop f b

nextWorld :: (MPos -> Bool) -> World -> Maybe World
nextWorld stop (m, w) = (\p -> (m, M.insert p Sand w)) <$> iterateMaybe stop (moveSand (m, w)) (500, 0)

nextWorld2 :: (M.Map MPos Block -> Bool) -> World -> Maybe World
nextWorld2 stop (m, w) = do
  res <- iterateMaybe (const False) (moveSand (m, w)) (500, 0)
  let world = M.insert res Sand w
  if stop world then Nothing else return (m, world)

part1 :: World -> Int
part1 (m, w) = pred . length . takeWhile isJust . iterate (>>= nextWorld ((>= m) . snd)) . Just $ (m, w)

part2 :: World -> Int
part2 (m, w) = length . takeWhile isJust . iterate (>>= nextWorld2 (M.member (500, 0))) . Just $ (m, w)

main :: IO ()
main = do
    input <- parseWorld <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
