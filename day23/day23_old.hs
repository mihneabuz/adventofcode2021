import qualified Data.PQueue.Prio.Min as MinQ
import qualified Data.IntSet as Set
import Data.Maybe (fromJust)

data Amphi = A | B | C | D | E deriving (Eq)
data Room  = Small Amphi Amphi | Big Amphi Amphi Amphi Amphi deriving (Show, Eq)
type Rooms = (Room, Room, Room, Room)
data Board = Board Rooms [Amphi] Int

instance Show Amphi where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "."

cost :: Amphi -> Int
cost A = 1
cost B = 10
cost C = 100
cost D = 1000
cost E = 0

pos :: Amphi -> Int
pos A = 1
pos B = 2
pos C = 3
pos D = 4
pos E = 5

top :: Room -> Amphi
top (Small x _) = x
top (Big x _ _ _) = x

isE :: Amphi -> Bool
isE E = True
isE _ = False

roomComplete :: Amphi -> Room -> Bool
roomComplete x (Small a b) = a == x && b == x

pathFromRoom :: Amphi -> Int -> [Amphi] -> [Amphi]
pathFromRoom amphi y = if x >= y then take (x - y + 1) . drop y else take (y - x) . drop (x + 1)
    where x = pos amphi

pathCost :: Amphi -> Amphi -> Int -> Int
pathCost amphiRoom amphi y = ((abs (x - y + fix1)) * 2 - fix2) * c
    where (x, c) = (pos amphiRoom, cost amphi)
          fix1 = if x >= y           then 1 else 0
          fix2 = if y == 0 || y == 6 then 1 else 0

place :: Int -> Amphi -> [Amphi] -> [Amphi]
place pos x xs = (take pos xs) ++ (x : drop (pos + 1) xs)

instance Show Board where
    show (Board (ra, rb, rc, rd) [h0, h1, h2, h3, h4, h5, h6] cost) =
        "\n#############\n" ++ 
        "#" ++ show h0 ++ show h1 ++ "." ++ show h2 ++ "." ++ show h3 ++ "." ++ show h4 ++ "." ++ show h5 ++ show h6 ++ "#\n" ++
        "###" ++ show (top ra) ++ "#" ++ show (top rb) ++ "#" ++ show (top rc) ++ "#" ++ show (top rd) ++ "###\n" ++
        showRooms (ra, rb, rc, rd) ++
        "  #########\n" ++
        "  Cost: " ++ show cost ++ "\n"
            where 
                showRooms (Small _ a, Small _ b, Small _ c, Small _ d) = 
                    "  #" ++ show a ++ "#" ++ show b ++ "#" ++ show c ++ "#" ++ show d ++ "#\n"

instance Eq Board where
    (==) b1 b2 = (==) (hashBoard b1) (hashBoard b2)

instance Ord Board where
    compare b1 b2 = compare (hashBoard b1) (hashBoard b2)

boardCost :: Board -> Int
boardCost (Board _ _ cost) = cost

hashBoard :: Board -> Int
hashBoard (Board (ra, rb, rc, rd) hall _) = sum . zipWith (*) [5 ^ i | i <- [0..]] . map ((-) 1 . pos) $ (hall ++ allAmphis)
    where allAmphis = concatMap amphis [ra, rb, rc, rd]
          amphis (Small x y) = [x, y]
          amphis (Big x y z w) = [x, y, z, w]

getRoom :: Amphi -> Rooms -> Room
getRoom A (ra, _, _, _) = ra
getRoom B (_, rb, _, _) = rb
getRoom C (_, _, rc, _) = rc
getRoom D (_, _, _, rd) = rd

fixBoard :: Board -> Board
fixBoard (Board (ra, rb, rc, rd) hall c) = (Board (ra1, rb1, rc1, rd1) hall (c+ ca + cb + cc + cd))
    where 
        (ra1, ca) = fixRoom A ra
        (rb1, cb) = fixRoom B rb
        (rc1, cc) = fixRoom C rc
        (rd1, cd) = fixRoom D rd

        fixRoom amph (Small top bot)
          | top == E && bot /= amph = (Small bot E, cost bot)
          | top == amph && bot == E = (Small E top, cost top)
          | otherwise = (Small top bot, 0)

fromRoomMoves :: Amphi -> Board -> [Board]
fromRoomMoves E _ = []
fromRoomMoves x board@(Board rooms hall cost)
  | roomComplete x room || top room == E = []
  | otherwise = nextBoards
    where 
        room = getRoom x rooms
        (toMove, newRooms) = popRoom x rooms
        nextBoards = map makeMoveTo . filter (\h -> all isE $ pathFromRoom A h hall) $ [0..6]
        makeMoveTo h = Board newRooms (place h toMove hall) (cost + pathCost x toMove h)

        popRoom A (ra, rb, rc, rd) = (top ra, (emptyTop ra, rb, rc, rd))
        popRoom B (ra, rb, rc, rd) = (top rb, (ra, emptyTop rb, rc, rd))
        popRoom C (ra, rb, rc, rd) = (top rc, (ra, rb, emptyTop rc, rd))
        popRoom D (ra, rb, rc, rd) = (top rd, (ra, rb, rc, emptyTop rd))

        emptyTop (Small _ x) = (Small E x)
        emptyTop (Big _ x y z) = (Big E x y z)


toRoomMoves :: Board -> [Board]
toRoomMoves board@(Board rooms hall cost) = map makeMove . filter predicate . zip [0..] $ hall
    where 
        canMoveToRoom x (Small a b) = a == E && (b == E || b == x)
        validPath x xs = (head xs == x && all isE (tail xs)) || (all isE (init xs) && last xs == x)
        predicate (h, x) = x /= E && (canMoveToRoom x . getRoom x $ rooms) && (validPath x $ pathFromRoom x h hall) 
        makeMove (h, x) = Board (pushRoom x rooms) (place h E hall) (cost + pathCost x x h)

        pushRoom A (ra, rb, rc, rd) = (push A ra, rb, rc, rd)
        pushRoom B (ra, rb, rc, rd) = (ra, push B rb, rc, rd)
        pushRoom C (ra, rb, rc, rd) = (ra, rb, push C rc, rd)
        pushRoom D (ra, rb, rc, rd) = (ra, rb, rc, push D rd)

        push x (Small _ y) = Small x y
        push x (Big _ y z w) = Big x y z w

step :: Board -> [Board]
step board = map fixBoard (toMoves ++ fromMoves)
    where fromMoves = concatMap (flip fromRoomMoves board) [A, B, C, D]
          toMoves   = toRoomMoves board

boardComplete :: Board -> Bool
boardComplete (Board (ra, rb, rc, rd) _ _) = 
    roomComplete A ra && roomComplete B rb &&
    roomComplete C rc && roomComplete D rd

solve :: Board -> Int
solve board = boardCost $ dijsktra (Set.singleton (hashBoard board)) (MinQ.singleton 0 board)
    where 
        dijsktra :: Set.IntSet -> MinQ.MinPQueue Int Board -> Board
        dijsktra seen queue = if boardComplete board then board else dijsktra (Set.insert (hashBoard board) seen) newQueue
            where board = snd . fromJust $ MinQ.getMin queue
                  next = filter (\b -> Set.notMember (hashBoard b) seen) $ step board
                  newQueue = foldl (\acc b -> MinQ.insert (boardCost b) b acc) (MinQ.deleteMin queue) $ next

day23 :: IO ()
day23 = do
    print . solve $ input1
    return ()

main = day23

h0 = replicate 7 E
h1 = place 0 B . place 1 A . place 4 B . place 6 C $ h0
input1 = Board ((Small B D), (Small B A), (Small C A), (Small D C)) h0 0
input2 = Board ((Small B A), (Small C D), (Small B C), (Small D A)) h0 0
input3 = Board ((Small E A), (Small E E), (Small E C), (Small D D)) h1 0
input4 = Board ((Small B A), (Small A B), (Small D C), (Small D C)) h0 0
