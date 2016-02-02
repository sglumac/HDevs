module Main where

import Test.Tasty

import Test.Simulator (simulatorTests)
import Test.ArrowLaws (arrowLaws)
import Test.Atomic (atomicTests)


suite :: TestTree
suite = testGroup "Tests HDevs"
    [ simulatorTests
    , arrowLaws
    , atomicTests ]

main :: IO ()
main = defaultMain suite
