import Data.List.Split
import qualified Data.Set as Set

type Coord = (Int, Int)
type Cuboid = [Coord]
data State = On | Off deriving (Show, Eq)
data Instruction = Instruction State Coord Coord Coord deriving (Show, Eq)

parse :: String -> [Instruction]
parse str = map (\[state, coords] -> (instruction state (parseCoords coords))) . map (splitOn " ") . lines $ str
    where parseCoords = map ((\[x, y] -> (read x :: Int, read y :: Int)) . splitOn "..") . map (drop 2) . splitOn ","
          instruction "on"  [x, y, z] = Instruction On  x y z
          instruction "off" [x, y, z] = Instruction Off x y z

solve1 :: [Instruction] -> Int
solve1 input = Set.size $ foldl helper Set.empty input
    where
        lb = -50 :: Int
        ub =  50 :: Int

        cubes (x0,x1) (y0,y1) (z0,z1) = 
            [(x,y,z) | x <- [(max lb x0) .. (min ub x1)],
                       y <- [(max lb y0) .. (min ub y1)],
                       z <- [(max lb z0) .. (min ub z1)]]

        helper acc (Instruction state x y z) = case state of
                On  -> Set.union      acc (Set.fromList $ cubes x y z)
                Off -> Set.difference acc (Set.fromList $ cubes x y z)

segOverlap :: Coord -> Coord -> [Coord]
segOverlap (x0, x1) (x0', x1') 
  | x0 < x0' && x1 > x1' = [(x0, x0' - 1), (x0', x1'), (x1' + 1, x1)]
  | x0 < x0'  = [(x0, x0' - 1), (x0', x1)]
  | x1 > x1'  = [(x0, x1'), (x1' + 1, x1)]
  | otherwise = [(x0, x1)]

removeOverlap :: Instruction -> Instruction -> [Instruction]
removeOverlap (Instruction _ x' y' z') (Instruction _ x y z) = 
    if and [hasOverlap x x', hasOverlap y y', hasOverlap z z']
       then [ Instruction On xr yr zr |
                xr <- segOverlap x x',
                yr <- segOverlap y y',
                zr <- segOverlap z z',
                not $ and [hasOverlap xr x', hasOverlap yr y', hasOverlap zr z']
            ]
       else [Instruction On x y z]

    where hasOverlap (x0, x1) (x0', x1') = x0 <= x1' && x0' <= x1

applyInstruction :: [Instruction] -> Instruction -> [Instruction]
applyInstruction acc ins@(Instruction state _ _ _) = 
    let acc' = concatMap (removeOverlap ins) acc in
        case state of
            On  -> ins : acc'
            Off -> acc'

solve2 :: [Instruction] -> Int
solve2 = sum . map volume . foldl applyInstruction [] 
    where volume (Instruction On x y z) = foldl (*) 1 . map (\(x0, x1) -> x1 - x0 + 1) $ [x, y, z]

day22 :: IO ()
day22 = do
    input <- readFile "input"
    print . solve1 . parse $ input
    print . solve2 . parse $ input
