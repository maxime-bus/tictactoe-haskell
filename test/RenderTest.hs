module RenderTest where

import Test.HUnit

import Grid.Grid
import Grid.Render

test1 = TestCase $ do
    assertEqual "" ("X") (renderCase Cross)
    assertEqual "" ("O") (renderCase Circle)
    assertEqual "" (" ") (renderCase EmptyCase)

tests = TestList [test1]

main = runTestTT tests
