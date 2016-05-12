module Grid where

import Utils
import Data.List

type SideSize = Int 
type Row = Int
type Column = Int

data Shape = Cross | Circle deriving (Show)
data Grid = Grid [Maybe Shape] SideSize deriving (Show)
data Position = Position Row Column deriving (Show)

renderPiece :: Maybe Shape -> String
renderPiece (Just Circle) = "0"
renderPiece (Just Cross) = "X"
renderPiece Nothing = " "

renderPieces :: [Maybe Shape] -> [String]
renderPieces = map renderPiece

renderGrid :: Grid -> [String]
renderGrid (Grid pieces sideSize) = mapEvery sideSize (++"|\n") $ map ("|" ++) $ renderPieces pieces

renderGrid' :: Grid -> String
renderGrid' grid = intercalate "" $ renderGrid grid

initGrid :: Int -> Grid
initGrid n = Grid (map (\ _ -> Nothing) [1..n^2]) n

updateGrid :: Grid -> Position -> Maybe Shape -> Grid
updateGrid (Grid pieces size) position piece = Grid newPieces size
    where
        index = computeIndex position size
        newPieces = init' (take index pieces) ++ [piece] ++ (drop index pieces)

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
computeIndex :: Position -> SideSize -> Int
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
