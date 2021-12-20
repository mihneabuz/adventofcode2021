import Data.List.Split

type Map = [[Char]]

parse :: String -> ([Char], Map)
parse str = (alg, lines img)
    where [alg, img] = splitOn "\n\n" str

pad :: Char -> Map -> Map
pad c m = map (\line -> [c, c] ++ line ++ [c, c]) $ ([padLine, padLine] ++ m ++ [padLine, padLine])
    where padLine = replicate (length . head $ m) c

slices :: Map -> [[[Char]]]
slices m = map (\i -> helper . take 3 . drop i $ m) [0..length m - 3]
    where helper rows = map (\j -> concatMap (take 3 . drop j) rows) [0..length (head rows) - 3]

lookup' :: [Char] -> [Char] -> Char
lookup' alg str = alg !! (number str)
    where number = sum . zipWith (\e c -> if c == '#' then e else 0) [2 ^ i | i <- [0..]] . reverse

enhance :: [Char] -> (Map, Char) -> (Map, Char)
enhance alg (m, padding) = (enhanced, nextPadding)
    where enhanced = map (map (lookup' alg)) . slices . pad padding $ m
          nextPadding = lookup' alg (replicate 9 padding)

solve :: Int -> ([Char], Map) -> Int
solve iters (alg, m) = count . fst . last . take (iters+1) . iterate (enhance alg) $ (m, '.')
    where count = length . filter (== '#') . concat

day20 :: IO ()
day20 = do
    input <- readFile "input"
    print . solve 2 . parse $ input
    print . solve 50 . parse $ input
