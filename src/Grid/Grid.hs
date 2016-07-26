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

updateGrid :: Grid -> Position -> Case -> Grid
updateGrid (Grid cases size) position c = Grid newCases size
    where
        index = computeIndex position size
        newCases = init' (take index cases) ++ [c] ++ (drop index cases)

getCaseState :: Position -> Grid -> Case
getCaseState pos (Grid cases size) = cases !! ((computeIndex pos size) - 1)

--  corner  |         |  corner
--   top    |   top   |   top
--   left   |         |  right
-- ---------|---------|--------

--          |.|.|.|.|.|
--          |.|.|.|.|.|
--   left   |.|.|.|.|.|  right
--          |.|.|.|.|.|
--          |.|.|.|.|.|

-- ---------|---------|--------
--  corner  |         |  corner
--  bottom  | bottom  |  bottom
--   left   |         |  right

-- TODO : find a way to improve this code, if possible.
computeIndex :: Position -> Size -> Int
computeIndex (Position row column) size
    | row <= 0 && column <= 0       = 1                             -- corner top left
    | row <= 0 && column <= size    = computeIndex' 1 column        -- top
    | row <= 0 && column > size     = computeIndex' 1 size          -- corner top right
    | row <= size && column > size  = computeIndex' row size        -- right
    | row > size && column > size   = computeIndex' size size       -- corner bottom right
    | row > size && column <= size  = computeIndex' size column     -- bottom
    | row > size && column <= 0     = computeIndex' size 1          -- corner bottom left
    | row <= size && column <= 0    = computeIndex' row 1           -- left
    | otherwise                     = computeIndex' row column      -- inside the grid
    where
        computeIndex' r c = size*(r-1) + c
