import Data.List.Split
import Data.List

parse :: String -> ((Int, Int), (Int, Int))
parse input = pack . map (pack . map read . splitOn ".." . drop 2) . splitOn ", " . drop 13 $ input
    where pack (x:y:_) = (x, y)

step :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
step ((x, y), (dx, dy)) = ((x + dx, y + dy), (drag dx, dy - 1))
    where drag x
           | x > 0 = x - 1
           | x < 0 = x + 1
           | otherwise = x

simmulate :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
simmulate range (pos, ds)
  | hit range (pos, ds) = True
  | over range (pos, ds) = False
  | otherwise = simmulate range (step (pos, ds))
    where
        between (x, y) a = x <= a && a <= y
        hit (xrange, yrange) ((x, y), _) = between xrange x && between yrange y
        over ((_, x1), (y0, _)) ((x, y), _) = x > x1 || y < y0

solve1 :: ((Int, Int), (Int, Int)) -> Int
solve1 input@((x0, x1), (y0, y1)) = highest . filter (\(dx, dy) -> simmulate input ((0, 0), (dx, dy))) $ possible
    where lowerdx = floor . sqrt . (*2) $ fromIntegral x0
          upperdx = ceiling . sqrt . (*2) $ fromIntegral x1
          possible = [(dx, dy) | dx <- [lowerdx..upperdx], dy <- [(abs y1)..(abs y0)]]
          highest = maximum . map (\(_, dy) -> dy * (dy + 1) `div` 2)

solve2 :: ((Int, Int), (Int, Int)) -> Int
solve2 input@((x0, x1), (y0, y1)) = length . filter (\(dx, dy) -> simmulate input ((0, 0), (dx, dy))) $ possible
    where lowerdx = floor . sqrt . (*2) $ fromIntegral x0
          possible = [(dx, dy) | dx <- [lowerdx..x1], dy <- [y0..(abs y0)]]

day17 :: IO ()
day17 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
