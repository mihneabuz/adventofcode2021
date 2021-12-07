input :: String
input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

data Line = Vertical Int Int Int | Horizontal Int Int Int | Diagonal Int Int Int Int deriving (Show, Eq)

instance Read Line where
    readsPrec a str = do
        (startX, rest1) <- lex str
        (comma1, rest2) <- lex rest1
        (startY, rest3) <- lex rest2
        (arrow, rest4)  <- lex rest3
        (endX, rest5)   <- lex rest4
        (comma2, rest6) <- lex rest5
        (endY, rest7)   <- lex rest6

        let x1 = read startX :: Int
        let y1 = read startY :: Int
        let x2 = read endX :: Int
        let y2 = read endY :: Int

        let result = case x1 == x2 of
                      True  -> Horizontal x1 (min y1 y2) (max y1 y2)
                      False -> case y1 == y2 of
                                 True  -> Vertical y1 (min x1 x2) (max x1 x2)
                                 False -> Diagonal x1 y1 x2 y2
    
        return (result, rest7)

solve :: [Line] -> Int
solve lines = sum . map (flip leastTwo lines) $ positions
    where
        (maxX, maxY) = findMargins lines
        positions = concat $ map (zip [0..maxX] . take maxX .repeat) [0..maxY]

findMargins :: [Line] -> (Int, Int)
findMargins = foldl helper (0, 0)
    where
        helper (maxX, maxY) (Vertical x y1 y2) = (max maxX x, maximum [maxY, y1, y2])
        helper (maxX, maxY) (Horizontal y x1 x2) = (maximum [maxX, x1, x2], max maxY y)
        helper (maxX, maxY) (Diagonal x1 y1 x2 y2) = (maximum [maxX, x1, x2], maximum [maxY, y1, y2])

contains :: (Int, Int) -> Line -> Int
contains (i, j) (Horizontal x y1 y2) = if i == x && y1 <= j && j <= y2 then 1 else 0
contains (i, j) (Vertical y x1 x2)   = if j == y && x1 <= i && i <= x2 then 1 else 0
contains (x, y) (Diagonal x1 y1 x2 y2) = if between x x1 x2 && between y y1 y2 && 
                                            (y - y1) * (x2 - x1) == (x - x1) * (y2 - y1) then 1 else 0

leastTwo :: (Int, Int) -> [Line] -> Int
leastTwo point lines = if (sum . map (contains point) $ lines) > 1 then 1 else 0

between :: Int -> Int -> Int -> Bool
between x a b = x >= (min a b) && x <= (max a b)

readLines :: FilePath -> IO [Line]
readLines name = do
    content <- readFile name
    return . map read . lines $ content

day5 :: IO ()
day5 = do
    input <- readLines "input"
    let result = solve input
    print result
    return ()
