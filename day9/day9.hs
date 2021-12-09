import Data.List

input :: String
input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

parse :: String -> [[Int]]
parse = map (map (\c -> read [c] :: Int)) . lines

solve1 :: [[Int]] -> Int
solve1 heightmap = sum . map smallest $ points
    where 
        n = length heightmap
        m = length (heightmap !! 0)

        isValid (x, y) = 0 <= x && x < n && 0 <= y && y < m
        getVal (x, y) = if isValid (x, y) then (heightmap !! x) !! y else -1
        getNeigh (x, y) = filter (/= -1) . map getVal $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

        smallest (x, y) = if (all (> val) $ getNeigh (x, y)) then val + 1 else 0
                             where val = getVal (x, y)

        points = [(a, b) | a <- [0..n - 1], b <- [0..m - 1]]


solve2 :: [[Int]] -> Int
solve2 heightmap = foldr (*) 1 . take 3 . reverse . sort . map (\x -> length . dfs [x] $ []) . filter smallest $ points
    where 
        n = length heightmap
        m = length (heightmap !! 0) 

        isValid (x, y) = 0 <= x && x < n && 0 <= y && y < m
        getVal (x, y) = if isValid (x, y) then (heightmap !! x) !! y else -1
        getNeigh (x, y) = filter (/= -1) . map getVal $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

        smallest (x, y) = all (> getVal (x, y)) $ getNeigh (x, y)

        dfs :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
        dfs [] partial = partial
        dfs ((x, y):points) partial = if valid then dfs ((x - 1, y):(x + 1, y):(x, y - 1):(x, y + 1):points) ((x,y):partial) else dfs points partial
            where valid = isValid (x, y) && getVal (x, y) /= 9 && not (elem (x, y) partial)

        points = [(a, b) | a <- [0..n - 1], b <- [0..m - 1]]


day9 :: IO ()
day9 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
