import qualified Data.Map as Map

import Data.Map ((!))
import Data.List
import Data.List.Split
import Data.Function

type Pair = (Char, Char)
type Template = [(Pair, Int)]
type Rules = Map.Map Pair [Pair]

parse :: String -> (String, [(String, String)])
parse input = (template, map (pack . splitOn " -> ") . filter (not . null) . splitOn "\n" $ rules)
    where [template, rules] = splitOn "\n\n" input
          pack (x:y:_) = (x, y)

initTemplate template = map (\xs -> (head xs, length xs)) $ pairs
    where pairs = group . sort . zip template $ tail template

initRules xs = Map.fromList . map entry $ xs
    where entry (s1, s2) = ((head s1, last s1), [(head s1, head s2), (head s2, last s1)])

step :: Rules -> Template -> Template
step rules template = combine . group . concatMap nextPairs $ template
    where nextPairs (p, c) = map (\pair -> (pair, c)) $ rules ! p
          group = groupBy ((==) `on` fst) . sortBy (compare `on` fst)
          combine = map (\xs -> (fst . head $ xs, sum . map snd $ xs))

occurances :: Template -> [(Char, Int)]
occurances = combine . group . concatMap (\((c1, c2), x) -> [(c1, x), (c2, x)])
    where group = groupBy ((==) `on` fst) . sortBy (compare `on` fst)
          combine = map (\xs -> (fst . head $ xs, div ((sum . map snd $ xs) + 1) 2))

solve :: Int -> (String, [(String, String)]) -> Int
solve n (template, rules) = maximum counts - minimum counts
    where counts = map snd . occurances . last . take (n+1) . iterate (step (initRules rules)) $ initTemplate template

day14 :: IO ()
day14 = do
    input <- readFile "input"
    print . solve 10 . parse $ input
    print . solve 40 . parse $ input
