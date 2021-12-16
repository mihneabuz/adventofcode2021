import Data.Char
import Data.Array
import Data.List

n = 9
m = 9

parse :: String -> Array (Int, Int) (Int, Bool)
parse input = array ((0, 0), (n, m)) [((y, x), (v, False)) | (y, row) <- zip [0..] elems, (x, v) <- zip [0..] row]
    where elems = map (map digitToInt) . lines $ input

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = 0 <= x && x <= n && 0 <= y && y <= m
          
neighs :: (Int, Int) -> [(Int, Int)]
neighs (x, y) = filter inBounds [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1]]

isFlashing :: (Int, Bool) -> Bool
isFlashing (x, y) = not y && x >= 10

hasFlashed :: (Int, Bool) -> Bool
hasFlashed = snd

resetFlashed :: (Int, Bool) -> (Int, Bool)
resetFlashed (x, True) = (0, False)
resetFlashed (x, False) = (x, False)

step :: (Array (Int, Int) (Int, Bool), Int) -> (Array (Int, Int) (Int, Bool), Int)
step (arr, acc) = count acc . step' . fmap (inc 1) $ arr
    where flashing arr = [(x, y) | (x, y) <- indices arr, isFlashing (arr ! (x, y))]
          flashed arr = map (\xs -> (head xs, length xs)) . group . sort . concat . map neighs . flashing $ arr

          inc k (x, y) = (x + k, y)
          count acc arr = (fmap resetFlashed arr, foldr (\x ac -> if hasFlashed x then ac + 1 else ac) acc arr)

          step' :: Array (Int, Int) (Int, Bool) -> Array (Int, Int) (Int, Bool)
          step' arr = if null . flashing $ arr then arr else recurse
              where recurse = step' (arr // incNeigh // markFlashed)
                    markFlashed = [((i, j), (0, True)) | (i, j) <- flashing arr]
                    incNeigh = [((i, j), inc k (arr ! (i, j))) | ((i, j), k) <- flashed arr]
        
solve1 :: Array (Int, Int) (Int, Bool) -> Int
solve1 arr = snd . last . take 101 . iterate step $ (arr, 0)

solve2 :: Array (Int, Int) (Int, Bool) -> Int
solve2 arr = helper . iterate step $ (arr, 0)
    where helper (x:y:xs) = if snd x + 100 == snd y then 1 else 1 + helper (y:xs)

day11 :: IO ()
day11 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
