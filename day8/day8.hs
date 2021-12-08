import Data.List.Split
import Data.List
import Data.Function

type Segment = Char
type SegmentMap = [(Char, [Char])]
type Line = ([String], [String])

segments = "abcdefg"
digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

parse :: String -> [Line]
parse = map (pack . map (splitOn " ") . splitOn " | ") . lines
    where pack (x:y:_) = (sortBy comp . map sort $ x, y)
          comp s1 s2 = compare (length s1) (length s2)

findAsocs :: [String] -> SegmentMap
findAsocs xs = complex 6 $ complex 5 $ simple 4 $ simple 7 $ simple 1 $ initial
    where
        initial = map (\c -> (c, segments)) $ segments

        simple :: Int -> SegmentMap -> SegmentMap 
        simple digit partialMap = updateMap from to partialMap
            where to = digits !! digit
                  from = head . filter ((==) (length to) . length) $ xs

        complex :: Int -> SegmentMap -> SegmentMap
        complex len partialMap = foldr (\(from, to) map -> updateMap from to map) partialMap $ zip (process xs) (process digits) 
            where process = map (concat . map (take 1)) . groupBy ((==) `on` length) . sortBy (compare `on` length) . group . sort . concat . filter ((==) len . length)

        updateMap :: [Char] -> [Char] -> SegmentMap -> SegmentMap
        updateMap from to partialMap = map helper partialMap
            where helper (c, asocs) = if c `elem` from then (c, asocs `intersect` to)
                                                       else (c, asocs \\ to)
    
decode :: Line -> [String]
decode (x, y) = map (sort . map (flip lookup' . findAsocs $ x)) y
    where lookup' c = head . snd . head . filter ((==) c . fst)

number :: [String] -> Int
number = sum . zipWith (*) [10 ^ x | x <- [0..]] . reverse . map (just . flip elemIndex digits)
    where just (Just x) = x

solve1 :: [Line] -> Int
solve1 = sum . map (\(_, xs) -> length . filter (flip elem [2,3,4,7] . length) $ xs)

solve2 :: [Line] -> Int
solve2 = sum . map (number . decode)

day8 :: IO ()
day8 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input

