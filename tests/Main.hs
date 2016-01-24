module Main where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import HDevs.Atomic
import Control.Arrow
import qualified Control.Category

import Data.Function (on)
import Data.List (sortBy)

import ArrowLaws (arrowLawsGroup)

simpleGainTest :: Double -> Double -> Bool
simpleGainTest k x = ys == [(k*x,t)]
    where
        t = 1.0
        sim = arr (k*)
        ys = runSimulator 10.0 sim [(x,t)]

connectedGainsTest :: Double -> Double -> Double -> Bool
connectedGainsTest k1 k2 x = ys == [(k1*k2*x,t)]
    where
        t = 1.0
        sim = arr (k1*) >>> arr (k2*)
        ys = runSimulator 10.0 sim [(x,t)]

identityCheck :: [Message Double] -> Bool
identityCheck msgs = runSimulator tMax idSim msgs' == msgs'
    where
        msgs' = sortBy (compare `on` snd) msgs
        tMax = if msgs == [] then 1.0 else maximum . map snd $ msgs
        idSim = Control.Category.id


suite :: TestTree
suite = testGroup "Tests HDevs"
    [ testGroup "QuickCheck"
        [ testProperty "Check if a simple gain amplifies the input" simpleGainTest
        , testProperty "Check if connected gain amplify the input" connectedGainsTest
        , testProperty "Check if identity simulator changes the input" identityCheck ]
    , arrowLawsGroup ]

main :: IO ()
main = defaultMain suite
