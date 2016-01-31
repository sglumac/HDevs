module Main where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import HDevs
import qualified Control.Category

import Data.Function (on)
import Data.List (sortBy)

import Test.Gains (gains)
import Test.ArrowLaws (arrowLaws)
import Test.Atomic (atomic)


suite :: TestTree
suite = testGroup "Tests HDevs"
    [ gains
    , arrowLaws
    , atomic ]

main :: IO ()
main = defaultMain suite
