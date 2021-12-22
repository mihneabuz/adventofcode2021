import Data.Maybe
import Data.List

import Data.Array
import Control.Monad

type Player = (Int, Int)
type Game = (Player, Player)

parse :: String -> (Int, Int)
parse str = ((read . last . words $ player1), (read . last . words $ player2))
    where [player1, player2] = lines str

wrap :: Int -> Int -> Int
wrap limit x = if x `mod` limit == 0 then limit else x `mod` limit

step :: Game -> Int -> Game
step (player1, player2) dice = (player2, move dice player1)
    where 
        roll x = wrap 100 x + wrap 100 (x + 1) + wrap 100 (x + 2)
        move dice (pos, score) = (newPos, score + newPos)
            where newPos = wrap 10 (pos + roll dice)

winnerTo :: Int -> Game -> Maybe Int
winnerTo limit ((_, score1), (_, score2))
  | score1 >= limit = Just 1
  | score2 >= limit = Just 2
  | otherwise = Nothing

solve1 :: (Int, Int) -> Int
solve1 (start1, start2) = helper ((start1, 0), (start2, 0)) 1
    where 
        helper :: Game -> Int -> Int
        helper game dice = case winnerTo 1000 game of
            Just 1 -> (dice - 1) * (snd . snd $ game)
            Just 2 -> (dice - 1) * (snd . fst $ game)
            Nothing -> helper (step game (wrap 100 dice)) (dice + 3)

solve2 :: (Int, Int) -> Int
solve2 (start1, start2) = uncurry max $ scores ! (start1, start2, 0, 0)
    where 
        add (a, b) (c, d) = (a + c, b + d)
        distances = map (\xs -> (head xs, length xs)) . group . sort $ sum <$> replicateM 3 [1..3]
        scores = listArray ((1, 1, 0, 0), (10, 10, 20, 20))
            [ foldr add (0, 0) (dp ix) | ix <- range ((1, 1, 0, 0), (10, 10, 20, 20))]

        dp (p1, p2, s1, s2) = map countWinners distances
            where 
                countWinners (d, n) = if s1 + k >= 21 then (n, 0) else (y * n, x * n)
                    where k = wrap 10 (p1 + d)
                          (x, y) = scores ! (p2, k, s2, s1 + k)

day21 :: IO ()
day21 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
