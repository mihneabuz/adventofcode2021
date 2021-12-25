input = "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"

chunks xs = zip3 ([last xs] ++ init xs) xs (tail xs ++ [head xs])

moveRight :: [[Char]] -> [[Char]]
moveRight = map (map updateRight . chunks)
    where
        updateRight ('>', '.',  _ ) = '>'
        updateRight ( _,  '>', '>') = '>'
        updateRight ( _,  '>', 'v') = '>'
        updateRight ( _,  '>', '.') = '.'
        updateRight ( _,  '.',  _ ) = '.'
        updateRight ( _,  'v',  _ ) = 'v'

moveDown :: [[Char]] -> [[Char]]
moveDown m = map (\(x, y, z) -> map updateDown $ zip3 x y z) $ chunks m
    where
        updateDown ('v', '.',  _ ) = 'v'
        updateDown ( _,  'v', '>') = 'v'
        updateDown ( _,  'v', 'v') = 'v'
        updateDown ( _,  'v', '.') = '.'
        updateDown ( _,  '.',  _ ) = '.'
        updateDown ( _,  '>',  _ ) = '>'

solve :: [[Char]] -> Int
solve = stalemate 1 . iterate step
    where step = moveDown . moveRight
          stalemate step (x:y:xs) = if x == y then step else stalemate (step + 1) (y:xs)

day25 :: IO ()
day25 = do
    input <- readFile "input"
    print . solve . lines $ input
