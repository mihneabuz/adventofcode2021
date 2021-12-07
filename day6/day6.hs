import Data.List.Split

day6 :: IO ()
day6 = do
    input <- readFile "input"
    let result1 = solve1 . parse $ input
    print result1
    let result2 = solve2 . parse $ input
    print result2

parse :: String -> [Int]
parse = map readInt . splitOn ","
    where readInt = \x -> read x :: Int

initial :: [Int] -> [Int]
initial xs = map (\x -> length . filter ( == x) $ xs) [0..8]

step :: [Int] -> [Int]
step xs = map update [0..8]
    where update 8 = head xs
          update 6 = xs !! 7 + head xs
          update x = xs !! (x + 1)

solve1 :: [Int] -> Int
solve1 = sum . (!! 80) . iterate step . initial

solve2 :: [Int] -> Int
solve2 = sum . (!! 256) . iterate step . initial
