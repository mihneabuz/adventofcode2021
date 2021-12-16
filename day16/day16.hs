import Data.Map (fromList, (!))
import Data.Char (digitToInt)
import Data.List.Split

data Packet = Packet Int Int Content deriving (Show)
data Content = Literal Int | Operator [Packet] deriving (Show)

hexMap = fromList [
    ('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011"),
    ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111"),
    ('8', "1000"), ('9', "1001"), ('A', "1010"), ('B', "1011"),
    ('C', "1100"), ('D', "1101"), ('E', "1110"), ('F', "1111")]

binToInt = sum . zipWith (*) [2 ^ i | i <- [0..]] . map digitToInt . reverse

boolToInt False = 0
boolToInt True = 1

parse :: String -> String
parse = concatMap (hexMap !) 

literal :: String -> (Content, String)
literal input = (Literal . binToInt . concatMap tail $ bytes, drop (length bytes * 5) input)
    where bytes = takeUntil ((== '1') . head) . chunksOf 5 $ input
          takeUntil pred = (\(f, s) -> f ++ [head s]) . span pred

operator :: String -> (Content, String)
operator input
  | head input == '0' = operator1 . tail $ input
  | head input == '1' = operator2 . tail $ input

operator1 :: String -> (Content, String)
operator1 input = (Operator (decodeList contentStr), rest)
    where size = binToInt . take 15 $ input
          (contentStr, rest) = splitAt size . drop 15 $ input

operator2 :: String -> (Content, String)
operator2 input = (Operator packets, rest)
    where count = binToInt . take 11 $ input
          (packets, rest) = decodeCount count [] . drop 11 $ input

decode :: String -> (Packet, String)
decode input = (Packet version t content, rest)
    where version = binToInt . take 3 $ input
          t = binToInt . take 3 . drop 3 $ input
          (content, rest) = case t of
                              4 -> literal . drop 6 $ input
                              _ -> operator . drop 6 $ input

decodeList :: String -> [Packet]
decodeList "" = []
decodeList input = packet : decodeList rest
    where (packet, rest) = decode input

decodeCount :: Int -> [Packet] -> String -> ([Packet], String)
decodeCount 0 packets rest = (reverse packets, rest)
decodeCount x packets input = decodeCount (x - 1) (packet:packets) rest 
    where (packet, rest) = decode input

versionSum :: Packet -> Int
versionSum (Packet v _ (Literal _)) = v
versionSum (Packet v _ (Operator packets)) = v + (sum . map versionSum $ packets)

compute :: Packet -> Int
compute (Packet _ _ (Literal x)) = x
compute (Packet _ 0 (Operator packets)) = sum . map compute $ packets
compute (Packet _ 1 (Operator packets)) = foldl (*) 1 . map compute $ packets
compute (Packet _ 2 (Operator packets)) = minimum . map compute $ packets
compute (Packet _ 3 (Operator packets)) = maximum . map compute $ packets
compute (Packet _ 5 (Operator packets)) = boolToInt . (\xs -> all ((>) (head xs)) (tail xs)) . map compute $ packets
compute (Packet _ 6 (Operator packets)) = boolToInt . (\xs -> all ((<) (head xs)) (tail xs)) . map compute $ packets
compute (Packet _ 7 (Operator packets)) = boolToInt . (\xs -> all ((==) (head xs)) (tail xs)) . map compute $ packets

solve1 :: String -> Int
solve1 = versionSum . fst . decode

solve2 :: String -> Int
solve2 = compute . fst . decode

day16 :: IO ()
day16 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
