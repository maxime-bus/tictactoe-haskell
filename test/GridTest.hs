module GridTest (gridTests) where

import Test.HUnit

import Grid.Grid

test1 = TestCase $ do
    let (Grid cases size) = initGrid 4
    assertEqual "For a size of 4, the number of cases should be 16" (16) (length cases)

gridTests = TestList [test1]
