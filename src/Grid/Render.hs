module Grid.Render where

import Data.List

import Grid.Grid
import Utils

renderCase :: Case -> String
renderCase Circle = "O"
renderCase Cross = "X"
renderCase EmptyCase = " "

renderCases :: [Case] -> [String]
renderCases = map renderCase

renderGrid :: Grid -> [String]
renderGrid (Grid cases size) = mapEvery size (++"|\n") $ map ("|" ++) $ renderCases cases

renderGrid' :: Grid -> String
renderGrid' grid = intercalate "" $ renderGrid grid
