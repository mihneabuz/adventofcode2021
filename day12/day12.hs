import Data.List
import Data.List.Split
import Data.Char
import Data.Tuple
import qualified Data.Map as Map

type Graph = Map.Map String [String]

input :: String
input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"

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

diverge :: Graph -> [String] -> [[String]]
diverge graph path = map (\node -> node:path) . filter (not . backtrack path) . adjacent graph . head $ path
    where backtrack path node = all isLower node && node `elem` path

paths :: Graph -> [[String]] -> [[String]]
paths graph xs = diverging ++ final
    where diverging = concat . map (diverge graph) . filter ((/= "end") . head) $ xs
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

solve1 :: [(String, String)] -> Int
solve1 edges = length . paths g . last . takeWhile (any ((/= "end") . head)) . iterate (paths g) $ [["start"]]
    where g = graph edges

solve2 :: [(String, String)] -> Int
solve2 edges = length . paths g . last . takeWhile (any ((/= "end") . head)) . iterate (paths2 g) $ [["start"]]
    where g = graph edges

day11 :: IO ()
day11 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
