module Test.ArrowLaws (arrowLaws) where

import HDevs
import qualified Control.Category

import Test.Utility

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)


categoryIdentity :: [Message Double] -> Bool
categoryIdentity msgs = outputsEqual msgs' idSim msgs'
    where
        msgs' = sortMsgs msgs
        idSim = Control.Category.id


arrowIdentity :: [Message Double] -> Bool
arrowIdentity msgs = outputsEqual msgs' (arr id) msgs'
    where
        msgs' = sortMsgs msgs


arrowDistributiveTest :: Double -> Double -> Bool
arrowDistributiveTest k1 k2 = True


arrowLaws :: TestTree
arrowLaws = testGroup "Arrow Laws"
    [ testProperty "Arrow identity test (arr id == id)" arrowIdentity
    , testProperty "Category identity test" categoryIdentity ]
