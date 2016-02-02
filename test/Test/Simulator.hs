module Test.Simulator (simulatorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Utility

import HDevs

simpleGain :: Assertion
simpleGain = assertMessagesApproxEqual ys [(20.0,1.0)]
    where
        t = 1.0
        k = 4.0
        x = 5.0
        sim = arr (k*)
        xs = [(x,t)]
        ys = runSimulator 10.0 sim [(5.0, t)]

connectedGains :: Assertion 
connectedGains = assertMessagesApproxEqual ys [(60.0,1.0)]
    where
        sim = arr (3.0*) >>> arr (4.0*)
        ys = runSimulator 10.0 sim [(5.0,1.0)]


simulatorTests :: TestTree
simulatorTests = testGroup "Simulator Tests"
    [ testCase "Check if a simple gain amplifies the input" simpleGain
    , testCase "Check if connected gain amplify the input" connectedGains ]

