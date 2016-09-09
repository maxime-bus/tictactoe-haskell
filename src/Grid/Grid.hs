module Grid.Grid where

import Data.List
import qualified Data.Map as Map

type Size = Int 
type Row = Int
type Column = Int
type Position = (Row, Column)

data Case = EmptyCase | Cross | Circle deriving (Show)
data Grid = Grid (Map.Map Position Case) Size deriving (Show)

initGrid :: Int -> Grid
initGrid size = Grid emptyGrid size where
    emptyGrid = Map.fromList [((x, y), EmptyCase) | x <- [1..size], y <- [1..size]]

updateGrid :: Grid -> Position -> Case -> Grid
updateGrid (Grid cases size) pos newCase = Grid newValue size
    where newValue = Map.adjust (\ _ -> newCase) pos cases
    -- Why the fuck do I need a function to update a key oO ?

getCaseState :: Position -> Grid -> Maybe Case
getCaseState pos (Grid cases _) = Map.lookup pos cases
