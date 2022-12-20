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

maybeMove :: M.Map MPos Block -> MPos -> MPos -> Maybe MPos
maybeMove w (x, y) (a, b) = case w M.!? p' of
  Nothing -> Just p'
  Just _ -> Nothing
  where
    p' = (x + a, y +b)

moveSand :: M.Map MPos Block -> MPos -> Maybe MPos
moveSand w a = mvDown <|> mvLeft <|> mvRight
  where
    mvDown = maybeMove w a (0, 1)
    mvLeft = maybeMove w a (-1, 1)
    mvRight = maybeMove w a (1, 1)

iterateMaybe :: (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
iterateMaybe stop f a = case f a of
  Nothing -> Just a
  Just b -> if stop b then Nothing else iterateMaybe stop f b

nextWorld :: World -> Maybe World
nextWorld (m, w) = (\p -> (m, M.insert p Sand w)) <$> iterateMaybe ((>= m) . snd) (moveSand w) (500, 0)

part1 :: World -> Int
part1 = pred . length . takeWhile isJust . iterate (>>= nextWorld) . Just

main :: IO ()
main = do
    input <- parseWorld <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
