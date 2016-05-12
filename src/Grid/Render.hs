module Grid.Render where

import Data.List

import Grid.Grid
import Utils

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
