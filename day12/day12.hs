import Data.List
import Data.List.Split
import Data.Char
import Data.Tuple
import qualified Data.Map as Map

type Graph = Map.Map String [String]

parse :: String -> [(String, String)]
parse = map (pack . splitOn "-") . lines
    where pack (x:y:_) = (x, y)

graph :: [(String, String)] -> Graph
graph = foldr helper Map.empty
    where helper edge = insert edge . insert (swap edge)
          insert (from, to) acc = case Map.lookup from acc of
                                    Nothing -> Map.insert from [to] acc
                                    Just xs -> Map.insert from (to:xs) acc
          
adjacent :: Graph -> String -> [String]
adjacent graph node = case Map.lookup node graph of
                        Nothing -> []
                        Just xs -> xs

diverge1 :: Graph -> [String] -> [[String]]
diverge1 graph path = map (\node -> node:path) . filter (not . backtrack path) . adjacent graph . head $ path
    where backtrack path node = all isLower node && node `elem` path

paths1 :: Graph -> [[String]] -> [[String]]
paths1 graph xs = diverging ++ final
    where diverging = concat . map (diverge1 graph) . filter ((/= "end") . head) $ xs
          final = filter ((== "end") . head) xs

diverge2 :: Graph -> [String] -> [[String]]
diverge2 graph path = map (\node -> node:path) . filter (not . backtrack path) . adjacent graph . head $ path
    where backtrack path "start" = "start" `elem` path
          backtrack path "end" = "end" `elem` path
          backtrack path node = all isLower node && node `elem` path &&
                (any ((== 2) . length) . group . sort . filter (all isLower) $ path)

paths2 :: Graph -> [[String]] -> [[String]]
paths2 graph xs = final ++ diverging
    where diverging = concat . map (diverge2 graph) . filter ((/= "end") . head) $ xs
          final = filter ((== "end") . head) xs

takeUntil pred = (\(f, s) -> f ++ [head s]) . span pred

solve1 :: [(String, String)] -> Int
solve1 edges = length . last . takeUntil (any ((/= "end") . head)) . iterate (paths1 g) $ [["start"]]
    where g = graph edges

solve2 :: [(String, String)] -> Int
solve2 edges = length . last . takeUntil (any ((/= "end") . head)) . iterate (paths2 g) $ [["start"]]
    where g = graph edges

day12 :: IO ()
day12 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
