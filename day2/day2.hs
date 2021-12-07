input :: String
input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

data Command = Forward Int | Down Int | Up Int deriving (Show, Eq)

instance Read Command where
    readsPrec a str = do
        (lexem, rest1) <- lex str
        (value, rest2) <- lex rest1
        case lexem of
            "forward" -> return ((Forward (read value)), rest2)
            "down"    -> return ((Down (read value)), rest2)
            "up"      -> return ((Up (read value)), rest2)
            _         -> return ((Forward 0), "")

readLines :: FilePath -> IO [Command]
readLines name = do
    content <- readFile name
    return . map read . lines $ content

commands :: [Command]
commands = map read . lines $ input

startPoz :: (Int, Int)
startPoz = (0, 0)

doCommand :: (Int, Int) -> Command -> (Int, Int)
doCommand (x, y) (Forward d) = (x + d, y)
doCommand (x, y) (Down d)    = (x, y + d)
doCommand (x, y) (Up d)      = (x, y - d)

solve :: (Int, Int) -> [Command] -> Int
solve start commands = mult . foldl doCommand start $ commands
    where mult (x, y) = x * y

day2_1 :: IO ()
day2_1 = do
    input <- readLines "input"
    let result = solve startPoz input
    print result
    return ()

startPoz2 :: (Int, Int, Int)
startPoz2 = (0, 0, 0)

doCommand2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
doCommand2 (x, y, aim) (Forward d) = (x + d, y + d * aim, aim)
doCommand2 (x, y, aim) (Down d)    = (x, y, aim + d)
doCommand2 (x, y, aim) (Up d)      = (x, y, aim - d)

solve2 :: (Int, Int, Int) -> [Command] -> Int
solve2 start commands = mult . foldl doCommand2 start $ commands
    where mult (x, y, _) = x * y

day2_2 :: IO ()
day2_2 = do
    input <- readLines "input"
    let result = solve2 startPoz2 input
    print result
    return ()
