module TestRunner where

import Test.HUnit

import RenderTest
import GridTest 

main = runTestTT $ TestList [renderTests, gridTests]
