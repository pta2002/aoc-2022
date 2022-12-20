import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Either
import Data.Maybe
import Control.Arrow
import Data.List (sortBy, find)

data Packet = Number Int | List [Packet] deriving (Show, Eq)

pktComp :: Packet -> Packet -> Maybe Bool
pktComp (Number a) (Number b)
  | a == b = Nothing
  | a < b = Just True
  | otherwise = Just False
pktComp (List (a:as)) (List (b:bs)) = case r of
    Nothing -> pktComp (List as) (List bs)
    Just n -> Just n
  where
    r = pktComp a b
pktComp (List []) (List (_:_)) = Just True
pktComp (List (_:_)) (List []) = Just False
pktComp (List []) (List []) = Nothing
pktComp (Number a) (List b) = pktComp (List [Number a]) (List b)
pktComp (List a) (Number b) = pktComp (List a) (List [Number b])

toOrder :: Maybe Bool -> Ordering
toOrder (Just True) = LT
toOrder (Just False) = GT

pktOrder :: Packet -> Packet -> Ordering
pktOrder a b = toOrder $ pktComp a b

parseNumber :: Parsec Void String Packet
parseNumber = Number <$> read <$> some digitChar

parseList :: Parsec Void String Packet
parseList = do
  single '['
  ns <- parsePacket `sepBy` single ','
  single ']'

  return $ List ns

parsePacket :: Parsec Void String Packet
parsePacket = parseNumber <|> parseList

parsePair :: Parsec Void String (Packet, Packet)
parsePair = do
  a <- parsePacket
  single '\n'
  b <- parsePacket
  return (a, b)

parseInput :: Parsec Void String [(Packet, Packet)]
parseInput = parsePair `sepBy` string "\n\n"

parse2 :: String -> [Packet]
parse2 = (++ [List [List [Number 2]], List [List [Number 6]]])
  . concat . map (\(a,b) -> [a,b]) . fromRight [] . runParser parseInput "input.txt"

myParse :: String -> [(Int, (Packet, Packet))]
myParse = zip [1..] . fromRight [] . runParser parseInput "input.txt"

part1 :: [(Int, (Packet, Packet))] -> Int
part1 = sum . map fst . filter ((== Just True) . snd) . map (id *** uncurry pktComp)

sort2 :: [Packet] -> [(Int, Packet)]
sort2 = zip [1..] . sortBy pktOrder

part2 :: [Packet] -> Int
part2 ps = a * b
  where
    sorted = sort2 ps
    a = fromMaybe 0 $ fst <$> find ((== List [List [Number 2]]) . snd) sorted
    b = fromMaybe 0 $ fst <$> find ((== List [List [Number 6]]) . snd) sorted

main :: IO ()
main = do
  input <- myParse <$> readFile "input.txt"
  input2 <- parse2 <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input2)
