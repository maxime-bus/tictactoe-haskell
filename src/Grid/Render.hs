module Grid.Render where

import Data.List

import Grid.Grid
import Utils

renderCase :: Case -> Char
renderCase Circle = 'O'
renderCase Cross = 'X'
renderCase EmptyCase = '-'

renderCases :: [Case] -> String
renderCases [] = ""
renderCases (x:xs) = renderCase x : renderCases xs

renderGrid :: Grid -> String
renderGrid (Grid cases size) = 
    let numbers = map ((++) . (++ " ") . show) [1..size]
        renderedCases = renderCases cases
        renderedRows = map(++"|\n") $ map ("|"++) $ map (intersperse '|') $ chunk (size) renderedCases
        numberedRows = zipWith ($) numbers renderedRows
        numberedColumns = "   " ++ foldr ($) "" numbers
        renderedGrid = (intercalate [] numberedRows) ++ numberedColumns
    in renderedGrid
