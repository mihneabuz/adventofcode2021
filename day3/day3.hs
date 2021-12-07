input :: String
input = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

solve :: [String] -> Int
solve input = (decimal gammaRate) * (decimal epsilonRate)
    where
        gammaRate = map mostCommon . cols $ input
        epsilonRate = flipBits gammaRate


cols :: [String] -> [String]
cols ([]:_) = []
cols xs = map head xs : cols (map tail xs)

mostCommon :: String -> Char
mostCommon xs = max . foldl counter (0, 0) $ xs
    where
        max (zeros, ones) = if zeros > ones then '0' else '1'
        counter (zeros, ones) char = case char of
                                    '0' -> (zeros + 1, ones)
                                    '1' -> (zeros, ones + 1)
                                    _   -> (zeros, ones)

flipBits :: String -> String
flipBits = map (\char -> if char == '0' then '1' else '0')

decimal :: String -> Int
decimal = foldl helper 0 . zip [0..] . reverse
    where helper = \acc (pow, char) -> if char == '1' then acc + 2 ^ pow else acc

readLines :: FilePath -> IO [String]
readLines name = do
    content <- readFile name
    return . lines $ content

day3_1 :: IO ()
day3_1 = do
    input <- readLines "input"
    let result = solve input
    print result
    return ()

solve2 :: [String] -> Int
solve2 input = (decimal . findOxygen $ input) * (decimal . findCO2 $ input)

findOxygen :: [String] -> String
findOxygen ([]:_) = []
findOxygen [x] = x
findOxygen xs = char : findOxygen (map tail . filter (\x -> head x == char) $ xs)
    where char = mostCommon . map head $ xs

findCO2 :: [String] -> String
findCO2 ([]:_) = []
findCO2 [x] = x
findCO2 xs = char : findCO2 (map tail . filter (\x -> head x == char) $ xs)
    where char = if (mostCommon . map head $ xs) == '0' then '1' else '0'

day3_2 :: IO ()
day3_2 = do
    input <- readLines "input"
    let result = solve2 input
    print result
    return ()
