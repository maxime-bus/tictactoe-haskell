module Grid.Grid where

import Data.List

import Utils

type Size = Int 
type Row = Int
type Column = Int

data Case = EmptyCase | Cross | Circle deriving (Show)
data Grid = Grid [Case] Size deriving (Show)
data Position = Position Row Column deriving (Show)

initGrid :: Int -> Grid
initGrid n = Grid (map (\ _ -> EmptyCase) [1..n^2]) n

updateGrid :: Grid -> Int -> Case -> Grid
updateGrid (Grid cases size) index c = Grid newCases size
    where
        newCases = init' (take index cases) ++ [c] ++ (drop index cases)

getCaseState :: Int -> Grid -> Case
getCaseState index (Grid cases size) = cases !! (index - 1)

computeIndex :: Position -> Size -> Maybe Int
computeIndex (Position row column) size 
    | notInGrid row column = Nothing
    | otherwise            = Just(size*(row - 1) + column)
    where
        notInGrid r c = null (filter (\ pos -> pos == (r,c)) allAvailablePositions)
        allAvailablePositions = (,) <$> [1..size] <*> [1..size]
