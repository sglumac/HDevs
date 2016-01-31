module Test.Gains (gains) where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import HDevs


simpleGain :: Double -> Double -> Bool
simpleGain k x = ys == [(k*x,t)]
    where
        t = 1.0
        sim = arr (k*)
        ys = runSimulator 10.0 sim [(x,t)]

connectedGains :: Double -> Double -> Double -> Bool
connectedGains k1 k2 x = ys == [(k1*k2*x,t)]
    where
        t = 1.0
        sim = arr (k1*) >>> arr (k2*)
        ys = runSimulator 10.0 sim [(x,t)]


gains :: TestTree
gains = testGroup "Gains"
    [ testProperty "Check if a simple gain amplifies the input" simpleGain
    , testProperty "Check if connected gain amplify the input" connectedGains ]

