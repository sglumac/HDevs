module Test.ArrowLaws (arrowLawsGroup) where

import HDevs

import Test.Utility

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)


arrowIdentityTest :: [Message Double] -> Bool
arrowIdentityTest msgs = outputsEqual msgs' (arr id) msgs'
    where
        msgs' = sortMsgs msgs

arrowDistributiveTest :: Double -> Double -> Bool
arrowDistributiveTest k1 k2 = True

arrowLawsGroup :: TestTree
arrowLawsGroup = testGroup "Arrow Laws"
    [ testProperty "Arrow identity test (arr id == id):" arrowIdentityTest ]
