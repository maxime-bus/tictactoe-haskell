module RenderTest (renderTests) where

import Test.HUnit

import Grid.Grid
import Grid.Render

test1 = TestCase $ do
    assertEqual "Cross symbol should be 'X'" ("X") (renderCase Cross)
    assertEqual "Circle symbol should be 'O'" ("O") (renderCase Circle)
    assertEqual "Empty symbol should be ' '" (" ") (renderCase EmptyCase)

renderTests = TestList [test1]
