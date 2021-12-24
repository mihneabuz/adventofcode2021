import Algorithm.Search  ( dijkstra )
import Data.Array (listArray, (!))
import Data.Maybe (fromJust)

type Room  = [Char]
type Hall  = [Char]
type Board = ([Room], Hall) 
type Cost  = Int

roomIndex = listArray ('A', 'D') [0, 1, 2, 3]
moveCosts = listArray ('A', 'D') [1, 10, 100, 1000]
pathCosts = listArray ((0, 0), (3, 6)) $ concat [
                                          [3, 2, 2, 4, 6, 8, 9],
                                          [5, 4, 2, 2, 4, 6, 7],
                                          [7, 6, 4, 2, 2, 4, 5],
                                          [9, 8, 6, 4, 2, 2, 3]
                                               ]

replace :: Int -> a -> [a] -> [a]
replace i x xs = (take i xs) ++ (x : drop (i + 1) xs)

canReachHall :: Hall -> Int -> Int -> Bool
canReachHall hall r h = if r + 1 >= h then allEmpty h (r+1) else allEmpty (r+2) h
    where allEmpty left right = all (== '.') . take (right - left + 1) . drop left $ hall

canReachHome :: Hall -> Int -> Int -> Bool
canReachHome hall h r 
  | r + 1 >= h + 1 = allEmpty (h+1) (r+1)
  | h - 1 >= r + 2 = allEmpty (r+2) (h-1)
  | otherwise      = True
    where allEmpty left right = all (== '.') . take (right - left + 1) . drop left $ hall

toRoomMoves :: Board -> [Board]
toRoomMoves (rooms, hall) = [
                    (replace i (a : room) rooms,
                     replace j '.' hall)
                    | j <- [0..6]
                    , let a = hall !! j
                    , a /= '.'
                    , let i = roomIndex ! a
                    , let room = rooms !! i
                    , all (== a) room
                    , canReachHome hall j i
                    ]

toHallMoves :: Board -> [Board]
toHallMoves (rooms, hall) = [
                    (replace i (tail room) rooms, 
                     replace j (head room) hall)
                    | i <- [0..3]
                    , let room = rooms !! i
                    , not (null room)
                    , not (all (== ("ABCD" !! i)) room)
                    , j <- [0..6]
                    , canReachHall hall i j
                    ]

costMove :: Int -> Board -> Int -> Int -> Cost
costMove len (rooms, hall) roomNr hallNr = (pathCosts ! (roomNr, hallNr) + offset) * (moveCosts ! a)
    where as = rooms !! roomNr
          a  = if hall !! hallNr /= '.' then hall !! hallNr else head as
          offset = if hall !! hallNr /= '.' then len - 1 - length as else len - length as

costTransition :: Int -> Board -> Board -> Cost
costTransition len (rs, h) (rs', h') = costMove len (rs, h) roomNr hallNr
    where roomNr = firstDiff rs rs'
          hallNr = firstDiff h h'

          firstDiff (x:xs) (y:ys) = if x == y then 1 + firstDiff xs ys else 0

solve :: Int -> [Room] -> (Cost, [Board])
solve len rooms = fromJust $ dijkstra transitions (costTransition len) (solutionFound len) (rooms, ".......")
    where solutionFound len board = fst board == map (replicate len) "ABCD"
          transitions board = (toRoomMoves board) ++ (toHallMoves board)


day23 :: IO ()
day23 = do
  print . fst . solve 2 $ ["BD", "BA", "CA", "DC"]
  print . fst . solve 4 $ ["BDDD", "BCBA", "CBAA", "DACC"]

main = day23
