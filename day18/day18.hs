data Snail = Number Int | Pair Snail Snail deriving (Eq)

instance Read Snail where
    readsPrec a str = do
        (lexem, rest) <- lex str
        if lexem == "["
            then do
                (first, rest1) <- readsPrec a rest
                (comma, rest2) <- lex rest1
                (secnd, rest3) <- readsPrec a rest2
                (closing, rest4) <- lex rest3
                return ((Pair first secnd), rest4)
            else 
                return ((Number (read lexem)), rest)

instance Show Snail where
    show (Number x) = show x
    show (Pair s1 s2) = "[" ++ show s1 ++ "," ++ show s2 ++ "]"

parse :: String -> [Snail]
parse = map read . lines

reduce :: Snail -> Snail
reduce snail = case explode snail of
        Just newSnail -> reduce newSnail
        Nothing       -> case split snail of
            Just newSnail -> reduce newSnail
            Nothing       -> snail

explode :: Snail -> Maybe Snail
explode snail = fmap (\(s,_,_) -> s) $ helper snail 0
    where
        helper :: Snail -> Int -> Maybe (Snail, Int, Int)
        helper (Number x) depth = Nothing
        helper (Pair (Number x) (Number y)) depth
          | depth >= 4 = Just (Number 0, x, y)

        helper (Pair l r) depth = case helper l (depth + 1) of
                Just (snail, x, y) -> Just (Pair snail (addL y r), x, 0)
                Nothing            -> case helper r (depth + 1) of
                    Just (snail, x, y) -> Just (Pair (addR x l) snail, 0, y)
                    Nothing            -> Nothing

        addL 0 snail = snail
        addL a (Number x) = Number (x + a)
        addL a (Pair l r) = Pair (addL a l) r

        addR 0 snail = snail
        addR a (Number x) = Number (x + a)
        addR a (Pair l r) = Pair l (addR a r)

split :: Snail -> Maybe Snail
split (Number x) = if x >= 10 then Just (Pair (Number (x `div` 2)) (Number ((x + 1) `div` 2))) else Nothing
split (Pair l r) = case split l of
        Just snail -> Just (Pair snail r)
        Nothing    -> case split r of
            Just snail -> Just (Pair l snail)
            Nothing    -> Nothing

magnitude :: Snail -> Int
magnitude (Number x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

solve1 :: [Snail] -> Int
solve1 = magnitude . foldl1 addSnail
    where addSnail s1 s2 = reduce $ Pair s1 s2

solve2 :: [Snail] -> Int
solve2 snails = maximum . map magnitude $ [addSnail x y | x <- snails, y <- snails, x /= y]
    where addSnail s1 s2 = reduce $ Pair s1 s2

day18 :: IO ()
day18 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
