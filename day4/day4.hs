import Data.List.Split
import Data.List

input :: String
input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

parseInput :: String -> ([Int], [[[Int]]])
parseInput input = (choices, tables)
    where
        splitted = splitOn "\n\n" input
        readInt = \x -> read x :: Int
        readTable = map (map readInt . filter (not . null) . splitOn " ") . splitOn "\n"
        choices = map readInt . splitOn "," . head $ splitted
        tables = filter (not . null . head) . map readTable . tail $ splitted

solve :: [Int] -> [[[Int]]] -> Int
solve choices tables = case winner of
                         Just table -> score choice table
                         Nothing    -> solve (tail choices) nextTables
    where 
        choice = head choices
        nextTables = map (choose choice) tables
        winner = find check nextTables


readLines :: FilePath -> IO (String)
readLines name = do
    content <- readFile name
    return content

day4_1 :: IO ()
day4_1 = do
    input <- readLines "input"
    let (choices, tables) = parseInput input
    let result = solve choices tables
    print result
    return ()

cols :: [[Int]] -> [[Int]]
cols ([]:_) = []
cols xs = map head xs : cols (map tail xs)

choose :: Int -> [[Int]] -> [[Int]]
choose target = map (map (\x -> if x == target then -1 else x))

check :: [[Int]] -> Bool
check table = any (\row -> sum row == -1 * length row) $ table ++ cols table

score :: Int -> [[Int]] -> Int
score last table = last * foldl (\acc row -> acc + foldl (\acc x -> if x == -1 then acc else acc + x) 0 row) 0 table


solve2 :: [Int] -> [[[Int]]] -> Int
solve2 choices [table] = if check nextTable then score (head choices) nextTable else solve2 (tail choices) [nextTable]
    where nextTable = choose (head choices) table
solve2 choices tables = solve2 (tail choices) nextTables
    where nextTables = filter (not . check) . map (choose (head choices)) $ tables


day4_2 :: IO ()
day4_2 = do
    input <- readLines "input"
    let (choices, tables) = parseInput input
    let result = solve2 choices tables
    print result
    return ()
