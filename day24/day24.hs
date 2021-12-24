import Data.List.Split (chunksOf)
import ALU

data OP = V Int | Input Int | ADD OP OP | MUL OP OP | MOD OP OP | DIV OP OP | EQL OP OP

instance Show OP where
    show (V x) = show x
    show (Input x) = "INPUT" ++ show x
    show (ADD x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
    show (MUL x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
    show (MOD x y) = "(" ++ show x ++ "%" ++ show y ++ ")"
    show (DIV x y) = "(" ++ show x ++ "/" ++ show y ++ ")"
    show (EQL x y) = "(" ++ show x ++ "=" ++ show y ++ ")"

get :: Operand -> (OP, OP, OP, OP) -> OP
get (Number x) _ = (V x)
get (Reg 0) (w, _, _, _) = w
get (Reg 1) (_, x, _, _) = x
get (Reg 2) (_, _, y, _) = y
get (Reg 3) (_, _, _, z) = z

set :: Operand -> OP -> (OP, OP, OP, OP) -> (OP, OP, OP, OP) 
set (Reg 0) a (w, x, y, z) = (a, x, y, z)
set (Reg 1) a (w, x, y, z) = (w, a, y, z)
set (Reg 2) a (w, x, y, z) = (w, x, a, z)
set (Reg 3) a (w, x, y, z) = (w, x, y, a)

opTreeFromInitial :: (Int, (OP, OP, OP, OP)) -> [Instruction] -> (OP, OP, OP, OP)
opTreeFromInitial (i, regs) ins = snd $ foldl helper (i, regs) ins 
    where helper :: (Int, (OP, OP, OP, OP)) -> Instruction -> (Int, (OP, OP, OP, OP))
          helper (i, regs) (Inp r) = (i+1, set r (Input i) regs)
          helper (i, regs) (Add r1 r2) = (i, set r1 (ADD (get r1 regs) (get r2 regs)) regs)
          helper (i, regs) (Mul r1 (Number 0)) = (i, set r1 (V 0) regs)
          helper (i, regs) (Mul r1 r2) = (i, set r1 (MUL (get r1 regs) (get r2 regs)) regs)
          helper (i, regs) (Div r1 r2) = (i, set r1 (DIV (get r1 regs) (get r2 regs)) regs)
          helper (i, regs) (Mod r1 r2) = (i, set r1 (MOD (get r1 regs) (get r2 regs)) regs)
          helper (i, regs) (Eql r1 r2) = (i, set r1 (EQL (get r1 regs) (get r2 regs)) regs)

opTree = opTreeFromInitial (0, (V 0, V 0, V 0, V 0))

reduceOP :: OP -> OP
reduceOP (Input x) = (Input x)
reduceOP (V x)     = (V x)
reduceOP (ADD x y) = case (reduceOP x, reduceOP y) of
                       (V 0, r) -> r
                       (r, V 0) -> r
                       (r1, r2) -> (ADD r1 r2)
reduceOP (MUL x y) = case (reduceOP x, reduceOP y) of
                       (V 1, r) -> r
                       (r, V 1) -> r
                       (V 0, _) -> (V 0)
                       (_, V 0) -> (V 0)
                       (r1, r2) -> (MUL r1 r2)
reduceOP (DIV x y) = case (reduceOP x, reduceOP y) of
                       (V 0, _) -> (V 0)
                       (r, V 1) -> r
                       (r1, r2) -> (DIV r1 r2)
reduceOP (MOD x y) = case (reduceOP x, reduceOP y) of
                       (V 0, _) -> (V 0)
                       (_, V 1) -> (V 0)
                       (r1, r2) -> (MOD r1 r2)
reduceOP (EQL x y) = case (reduceOP x, reduceOP y) of
                       (V x, V y)     -> if x == y then (V 1) else (V 0)
                       (V x, Input y) -> if x > 9 || x < 0 then (V 0) else (EQL (Input y) (V x))
                       (Input y, V x) -> if x > 9 || x < 0 then (V 0) else (EQL (Input y) (V x))
                       (r1, r2)       -> (EQL r1 r2)

evalOn :: [Int] -> OP -> Int
evalOn input (Input x) = input !! x
evalOn input (V x)     = x
evalOn input (ADD x y) = evalOn input x + evalOn input y
evalOn input (MUL x y) = evalOn input x * evalOn input y
evalOn input (DIV x y) = evalOn input x `div` evalOn input y
evalOn input (MOD x y) = evalOn input x `mod` evalOn input y
evalOn input (EQL x y) = evalOn input x `eql` evalOn input y

input0 = "inp x\nmul x -1"
input1 = "inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2"
input2 = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 14\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 12\nmul y x\nadd z y"
input3 = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y"

f :: [Int] -> [[Instruction]] -> Int
f input ins' = foldl g 0 $ zip input (take 1 ins')
    where 
        g pers (x, ins) = evalOn [x] . reduceOP . (\(_,_,_,z) -> z) . opTreeFromInitial (0, (V 0, V 0, V 0, V pers)) $ ins

day23 :: IO ()
day23 = do
    input <- readFile "input"
    mapM_ print . map (reduceOP . (\(_,_,_,z) -> z) . opTree) . chunksOf 18 . parse $ input
    mapM_ print . map (reduceOP . (\(_,_,_,z) -> z) . opTreeFromInitial (0, (V 0, V 0, V 0, Input 99))) . chunksOf 18 . parse $ input

    print $ f ([5,9,6,9,2,9,9,4,9,9,4,9,9,8]) (chunksOf 18 . parse $ input)
    print $ f ([1,6,1,8,1,1,1,1,6,4,1,5,2,1]) (chunksOf 18 . parse $ input)
