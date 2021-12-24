module ALU where

type Registers = [Int]
data Operand = Reg Int | Number Int
data Instruction = Inp Operand         | 
                   Add Operand Operand | 
                   Mul Operand Operand |
                   Div Operand Operand |
                   Mod Operand Operand | 
                   Eql Operand Operand
                    deriving (Show)

instance Show Operand where
    show (Number x) = show x
    show (Reg x) = "r" ++ show x

parse :: String -> [Instruction]
parse = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction str = case head . words $ str of
        "inp" -> let reg = (words str) !! 1          in Inp (operand reg)
        "add" -> let [reg1, reg2] = tail (words str) in Add (operand reg1) (operand reg2)
        "mul" -> let [reg1, reg2] = tail (words str) in Mul (operand reg1) (operand reg2)
        "div" -> let [reg1, reg2] = tail (words str) in Div (operand reg1) (operand reg2)
        "mod" -> let [reg1, reg2] = tail (words str) in Mod (operand reg1) (operand reg2)
        "eql" -> let [reg1, reg2] = tail (words str) in Eql (operand reg1) (operand reg2)

operand :: String -> Operand
operand "w" = Reg 0
operand "x" = Reg 1
operand "y" = Reg 2
operand "z" = Reg 3
operand x   = Number (read x)

replace :: Int -> a -> [a] -> [a]
replace i x xs = (take i xs) ++ (x : drop (i + 1) xs)

fetch :: Operand -> Registers -> Int
fetch (Reg x) regs = regs !! x
fetch (Number x) _ = x

eql :: Int -> Int -> Int
eql x y = if x == y then 1 else 0

step :: (Registers, [Int]) -> Instruction -> (Registers, [Int])
step (regs, input) (Inp (Reg x)) = (replace x (head input) regs, tail input)
step (regs, input) (Add op1@(Reg x) op2) = (replace x (fetch op1 regs + fetch op2 regs) regs, input)
step (regs, input) (Mul op1@(Reg x) op2) = (replace x (fetch op1 regs * fetch op2 regs) regs, input)
step (regs, input) (Div op1@(Reg x) op2) = (replace x (fetch op1 regs `div` fetch op2 regs) regs, input)
step (regs, input) (Mod op1@(Reg x) op2) = (replace x (fetch op1 regs `mod` fetch op2 regs) regs, input)
step (regs, input) (Eql op1@(Reg x) op2) = (replace x (fetch op1 regs `eql` fetch op2 regs) regs, input)

run :: [Instruction] -> [Int] -> (Registers, [Int])
run ins input = foldl step ([0, 0, 0, 0], input) ins
