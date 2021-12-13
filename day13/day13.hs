import Data.List.Split
import Data.List
import Data.Function

parse :: String -> ([(Int, Int)], [(String, Int)])
parse input = (map (pack . map read . splitOn ",") coords, map ((\(x, y) -> (x, read y)) . pack . splitOn "=" . drop 11) $ folds)
    where (coords:folds:_) = map lines . splitOn "\n\n" $ input
          pack (x:y:_) = (x, y)

applyFold :: [(Int, Int)] -> (String, Int) -> [(Int, Int)]
applyFold dots ("x", x) = map head . group . sort . map (verticalFold x)   $ dots
applyFold dots ("y", y) = map head . group . sort . map (horizontalFold y) $ dots

verticalFold   k (x, y) = if k > x then (x, y) else (2 * k - x, y)
horizontalFold k (x, y) = if k > y then (x, y) else (x, 2 * k - y)

visualize :: [(Int, Int)] -> String
visualize dots = intercalate "\n" . map (\xs -> makeLine xs 0 max) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ dots 
    where max = maximum . map fst $ dots

makeLine :: [(Int, Int)] -> Int -> Int -> String
makeLine dots i m
  | i == m + 1 = []
  | otherwise = if i `elem` map fst dots then '█' : makeLine dots (i+1) m
                                         else ' ' : makeLine dots (i+1) m

solve1 :: ([(Int, Int)], [(String, Int)]) -> Int
solve1 (dots, folds) = length . applyFold dots . head $ folds

solve2 :: ([(Int, Int)], [(String, Int)]) -> String
solve2 (dots, folds) = visualize . foldl applyFold dots $ folds

day13 :: IO ()
day13 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    putStrLn . solve2 . parse $ input
