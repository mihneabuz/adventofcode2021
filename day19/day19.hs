import Data.List (sort, group, nub)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)

type Point = (Int, Int, Int)
addPoint (x, y, z) (a, b, c) = (x + a, y + b, z + c)
subPoint (x, y, z) (a, b, c) = (x - a, y - b, z - c)

data Axis = X | Y | Z deriving (Show, Eq)
rotate X (x, y, z) = (x, -z, y)
rotate Y (x, y, z) = (z, y, -x)
rotate Z (x, y, z) = (y, -x, z)

oritentations = flip (foldl (flip rotate)) <$> rotations
rotations = [ []
            , [X]
            , [Y]
            , [Z]
            , [X, X]
            , [X, Y]
            , [X, Z]
            , [Y, X]
            , [Y, Y]
            , [Z, Y]
            , [Z, Z]
            , [X, X, X]
            , [X, X, Y]
            , [X, X, Z]
            , [X, Y, X]
            , [X, Y, Y]
            , [X, Z, Z]
            , [Y, X, X]
            , [Y, Y, Y]
            , [Z, Z, Z]
            , [X, X, X, Y]
            , [X, X, Y, X]
            , [X, Y, X, X]
            , [X, Y, Y, Y]
            ]

parse :: String -> [[Point]]
parse = map parseScanner . splitOn "\n\n"

parseScanner :: String -> [Point]
parseScanner = map (\[x,y,z] -> (read x, read y, read z)) . map (splitOn ",") . tail . lines

correlate :: [Point] -> [Point] -> Maybe ([Point], Point)
correlate xs ys = if null correlated then Nothing else head correlated
    where
        correlated = filter isJust . map (\f -> translate xs . map f $ ys) $ oritentations

        translate :: [Point] -> [Point] -> Maybe ([Point], Point)
        translate xs ys = if matches >= 12 then Just (map (addPoint relative) ys, relative) else Nothing
            where diffs = sort $ [subPoint x y | x <- xs, y <- ys]
                  (matches, relative) = maximum . map (\xs -> (length xs, head xs)) . group $ diffs

combine :: [Point] -> [Point] -> [Point]
combine xs ys = nub $ ys ++ xs

longest :: [Point] -> Int
longest points = maximum [manhattan x y | x <- points, y <- points, x /= y]
    where manhattan (a, b, c) (x, y, z) = abs (a - x) + abs (b - y) + abs (c - z)

solve :: [[Point]] -> (Int, Int)
solve scanners = (\(x, y) -> (length x, longest y)) $ expandSolution (head scanners) (tail scanners) [(0,0,0)]
    where 
        expandSolution :: [Point] -> [[Point]] -> [Point] -> ([Point], [Point])
        expandSolution points [] relatives = (points, relatives)
        expandSolution points (x:xs) relatives = case correlate points x of
            Just (correlated, relative) -> expandSolution (combine points correlated) xs (relative:relatives)
            Nothing                     -> expandSolution points (xs ++ [x]) relatives

day19 :: IO ()
day19 = do
    input <- readFile "input"
    let (part1, part2) = solve . parse $ input
    print part1
    print part2
