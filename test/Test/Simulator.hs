module Test.Simulator (simulatorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Utility

import HDevs

simpleGain :: Assertion
simpleGain = assertMessagesApproxEqual 0.1 0.1 ys [(20.0,1.0),(24.0,2.0)]
    where
        sim :: Simulator Double Double
        sim = arr (4.0*)
        ys = runSimulator 10.0 sim [(5.0,1.0),(6.0,2.0)]

connectedGains :: Assertion 
connectedGains = assertMessagesApproxEqual 0.1 0.1 ys [(60.0,1.0)]
    where
        sim :: Simulator Double Double
        sim = arr (3.0*) >>> arr (4.0*)
        ys = runSimulator 10.0 sim [(5.0,1.0)]

connectedGains' :: Assertion
connectedGains' = assertMessagesApproxEqual 0.1 0.1 ys [(375.496597,1.0),(173.344997,2.0)]
    where
        sim :: Simulator Double Double
        sim = arr (3.01*) >>> arr (16.79*)
        ys = runSimulator 10.0 sim [(7.43,1.0),(3.43,2.0)]


simulatorTests :: TestTree
simulatorTests = testGroup "Simulator Tests"
    [ testCase "Check if a simple gain amplifies the input" simpleGain
    , testCase "Check if connected gains amplify the input" connectedGains
    , testCase "Check if connected gains amplify the input" connectedGains' ]

