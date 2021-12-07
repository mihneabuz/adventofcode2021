input :: [Int]
input = [199,200,208,210,200,207,240,269,260,263]

readLines :: FilePath -> IO [Int]
readLines name = do
    content <- readFile name
    return . map read . lines $ content

solve :: [Int] -> Int
solve xs = snd (foldl (\acc x -> if x > fst acc then (x, snd acc + 1) else (x, snd acc)) (0, -1) xs)

day1_1 :: IO ()
day1_1 = do
    input <- readLines "input.txt"
    let result = solve input
    print result
    return ()


solve_2 :: [Int] -> Int
solve_2 = solve . transform

transform :: [Int] -> [Int]
transform xs = map (\(a, b, c)  -> a + b + c) $ zip3 xs (drop 1 xs) (drop 2 xs)

day1_2 :: IO ()
day1_2 = do
    input <- readLines "input.txt"
    let result = solve_2 input
    print result
    return ()
