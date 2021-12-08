import Data.List.Split

parse :: String -> [Int]
parse = map readInt . splitOn ","
    where readInt = \x -> read x :: Int

solve1 :: [Int] -> Int
solve1 xs = minimum . map (\x -> sum . map (abs . subtract x) $ xs) $ [0 .. maximum xs]

solve2 :: [Int] -> Int
solve2 xs = minimum . map (\x -> sum . map (gauss . abs . subtract x) $ xs) $ [0 .. maximum xs]
    where gauss n = n * (n + 1) `div` 2

day7 :: IO ()
day7 = do
    input <- readFile "input"
    print (solve1 . parse $ input)
    print (solve2 . parse $ input)
