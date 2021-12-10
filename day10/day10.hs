import Data.List

data Result = Good | Error Char | Incomplete String deriving (Show, Eq)

input :: String
input = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

parse :: String -> [String]
parse = lines

isOpening :: Char -> Bool
isOpening = flip elem "([{<"

isClosing :: Char -> Bool
isClosing = flip elem ")]}>"

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'
match c = c

isMatching :: Char -> Char -> Bool
isMatching c1 c2 = (match c1) == c2

valueTable1 :: Char -> Int
valueTable1 ')' = 3
valueTable1 ']' = 57
valueTable1 '}' = 1197
valueTable1 '>' = 25137

valueTable2 :: Char -> Int
valueTable2 ')' = 1
valueTable2 ']' = 2
valueTable2 '}' = 3
valueTable2 '>' = 4

check :: String -> Result
check str = check' str []
    where 
        check' :: String -> String -> Result
        check' [] [] = Good
        check' [] xs = Incomplete xs
        check' (c:cs) stack = if isOpening c 
                                 then check' cs (c:stack)
                                 else if isMatching (head stack) c
                                         then check' cs (tail stack)
                                         else Error c

isError :: Result -> Bool
isError (Error _) = True
isError _ = False

solve1 :: [String] -> Int
solve1 = sum . map (\(Error c) -> valueTable1 c) . filter isError . map check

solve2 :: [String] -> Int
solve2 = middle . sort . map (foldl (\acc c -> acc * 5 + valueTable2 c) 0) . map (\(Incomplete xs) -> map match xs) . filter (not . isError) . map check
    where middle l = l !! ((length l) `div` 2)

day10 :: IO ()
day10 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
