import Data.Char (digitToInt)
import Data.Array
import Data.Function
import Data.List

import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as Map

type Point = (Int, Int)
type Cave = Array Point Int
type Paths = Map.Map Point Int
type Queue = Q.MinPQueue Int Point

parse :: String -> [[Int]]
parse input = map (map digitToInt) . lines $ input

extend :: Int -> [[Int]] -> [[Int]]
extend x m = extendRight . extendDown $ m
    where inc i x = if x + i > 9 then x + i - 9 else x + i
          extendDown m = concat $ map (\i -> map (map (inc i)) m) [0..x-1]
          extendRight m = foldr1 (\m acc -> zipWith (++) m acc) $ map (\i -> map (map (inc i)) m) [0..x-1]

cave :: [[Int]] -> Cave
cave arr = listArray ((0, 0), (length arr - 1, length (head arr) - 1)) . concat $ arr

neighs :: Cave -> Point -> [Point]
neighs arr (x, y) = filter valid [(x + dx, y + dy) | (dx,dy) <- [(-1, 0), (0, -1), (1, 0), (0,1)]]  
    where valid (x, y) = (\((x0, y0), (x1, y1)) -> x0 <= x && x <= x1 && y0 <= y && y <= y1) . bounds $ arr

dijkstra :: Cave -> Queue -> Paths -> Paths
dijkstra arr queue paths = if Q.null queue then paths else dijkstra arr newQueue newPaths 
    where
        point = snd $ Q.findMin queue
        value = arr ! point + paths Map.! point
        neighbours = filter (\point -> Map.notMember point paths || paths Map.! point > value) . neighs arr $ point

        newQueue = foldr (\point q -> Q.insert value point q) (Q.deleteMin queue) neighbours
        newPaths = foldr (\point m -> Map.insertWith min point value m) paths neighbours

shortest :: Cave -> Point -> Int
shortest cave point = paths Map.! point + cave ! point - cave ! (0, 0)
    where paths = dijkstra cave (Q.singleton 1 (0, 0)) (Map.singleton (0, 0) 0)

solve1 :: [[Int]] -> Int
solve1 m = shortest c (snd $ bounds c)
    where c = cave m 

solve2 :: [[Int]] ->  Int
solve2 = solve1 . extend 5

day15 :: IO ()
day15 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
