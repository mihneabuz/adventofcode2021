import Data.List.Split

input :: String
input = "16,1,2,0,4,2,7,1,2,14"

day7 :: IO ()
day7 = do
    input <- readFile "input"
    print (solve1 . parse $ input)
    print (solve2 . parse $ input)

parse :: String -> [Int]
parse = map readInt . splitOn ","
    where readInt = \x -> read x :: Int

solve1 :: [Int] -> Int
solve1 xs = minimum . map (\x -> sum . map (abs . subtract x) $ xs) $ [min .. max]
    where max = maximum xs
          min = minimum xs

solve2 :: [Int] -> Int
solve2 xs = minimum . map (\x -> sum . map (gauss . abs . subtract x) $ xs) $ [min .. max]
    where max = maximum xs
          min = minimum xs

gauss :: Int -> Int
gauss n = n * (n + 1) `div` 2
