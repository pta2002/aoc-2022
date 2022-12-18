listWindow :: Int -> [a] -> [[a]]
listWindow n xs
    | length xs > n = take n xs : listWindow n (tail xs)
    | otherwise = [xs]

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

part1 :: String -> Int
part1 = fst . head . filter snd . zip [4..] . map allUnique . listWindow 4

part2 :: String -> Int
part2 = fst . head . filter snd . zip [14..] . map allUnique . listWindow 14

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
