import Data.List.Split
import Data.List

type Point = (Int, Int)
type Line  = (Point, Point)

parse :: String -> Line
parse = pack . map (pack . map readInt . splitOn ",") . splitOn "->"
    where pack (x:y:_) = (x,y)
          readInt = \x -> read x :: Int

comp :: Ord a => a -> a -> Int
comp a b | b > a = 1
         | b < a = -1 
         | otherwise = 0

points :: Line -> [Point]
points ((x1,y1), (x2,y2)) = [(x1 + n*dx, y1 + n*dy) | n <- [0..maximum . map abs $ [x2 - x1, y2 - y1]]]
    where dx = comp x1 x2
          dy = comp y1 y2

solve :: [Line] -> Int
solve = length . filter (\xs -> length xs > 1) . group . sort . concat . map (points)

readLines :: FilePath -> IO [Line]
readLines name = do
    content <- readFile name
    return . map parse . lines $ content

day5 :: IO ()
day5 = do
    input <- readLines "input"
    let result = solve input
    print result
    return ()
