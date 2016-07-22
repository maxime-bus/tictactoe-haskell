module Main where

import Text.Read

import Utils
import Grid.Grid
import Grid.Render

data Player = PlayerX | PlayerO deriving (Eq, Show)

data GameState = GameOver | GameNotOver

main :: IO ()
main = do 
    grid <- fmap initGrid askGridSizeUntilSizeIsCorrect
    gameLoop grid GameNotOver PlayerX

askGridSizeUntilSizeIsCorrect :: IO (Int)
askGridSizeUntilSizeIsCorrect = do
    size <- putStrLn "Choose a grid size" >> getLine
    case readMaybe size of
        Just s  -> return s
        _       -> putStrLn "You just typed a wrong size." >> askGridSizeUntilSizeIsCorrect


askPlayerHisPosition :: Player -> IO (Position)
askPlayerHisPosition player = do
    pos <- putStrLn (show player ++ ", choose your position (row column) :") >> getLine
    case parsePosition pos of
        Just pos'   -> return pos'
        _           -> putStrLn "You just typed a wrong position." >> askPlayerHisPosition player

parsePosition :: String -> Maybe Position
parsePosition position = case words position of
    [a, b]    -> do
        row <- readMaybe a
        col <- readMaybe b
        return (Position row col)
    _         -> Nothing

checkWinner :: Grid -> Maybe Player
checkWinner (Grid pieces size) 
    | size == 3 = checkWinner' pieces
    | otherwise = checkWinner'' pieces

checkWinner' :: [Case] -> Maybe Player
checkWinner' _ = Just PlayerX

checkWinner'' :: [Case] -> Maybe Player
checkWinner'' _ = Just PlayerO

gameLoop :: Grid -> GameState -> Player -> IO ()
gameLoop _ GameOver _ = return ()
gameLoop currentGrid _ playerTurn = do
    putStrLn $ renderGrid' currentGrid
    position <- askPlayerHisPosition playerTurn
    case playerTurn of
        PlayerX -> do
            let gridUpdated = updateGrid currentGrid position Cross
            gameLoop gridUpdated GameNotOver PlayerO
	_       -> do
            let gridUpdated = updateGrid currentGrid position Circle
            gameLoop gridUpdated GameNotOver PlayerX
