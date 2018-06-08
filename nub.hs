import System.Console.ANSI
import System.IO.HiddenChar

data Position = Position { x :: Int, y :: Int } deriving (Show)

initiate :: IO Position 
initiate = do
    clearScreen
    let pos = Position { x = 20, y = 10 }
    setCursorPosition (y pos) (x pos) 
    putStrLn "a"
    return pos

inputLoop :: Position -> IO ()
inputLoop pos = do
    i <- getHiddenChar
    let pos' = keyCheck pos i
    setCursorPosition (y pos') (x pos')
    putStrLn [i]
    if i=='\ESC' then return ()
                 else inputLoop pos'

keyCheck :: Position -> Char -> Position 
keyCheck pos ch  
    | ch == 'j' = Position { x = (x pos), y = (y pos)+1 }
    | ch == 'k' = Position { x = (x pos), y = (y pos)-1 }
    | ch == 'h' = Position { x = (x pos)-1, y = (y pos) }
    | ch == 'l' = Position { x = (x pos)+1, y = (y pos) }
    | otherwise = Position { x = (x pos), y = (y pos) }

main :: IO ()
main = do
    pos <- initiate
    inputLoop pos
