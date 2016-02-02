module Test.ArrowLaws (arrowLaws) where

import HDevs
import qualified Control.Category

import Test.Utility

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)


categoryIdentity :: [Message Double] -> Bool
categoryIdentity msgs = messagesApproxEqual msgs' ys
    where
        msgs' = sortMsgs msgs
        tMax = maxTime msgs'
        ys = runSimulator tMax Control.Category.id msgs'


-- arr id = id 
arrowIdentity :: [Message Double] -> Bool
arrowIdentity msgs = messagesApproxEqual msgs' ys
    where
        msgs' = sortMsgs msgs
        tMax = maxTime msgs'
        ys = runSimulator tMax (arr id) msgs'


-- arr (h . g)  =  arr g >>> arr h
arrowDistributiveTest :: Double -> Double -> [Message Double] -> Bool
arrowDistributiveTest k1 k2 msgs = messagesApproxEqual xs ys
    where
        h = (k1*)
        g = (k2*)
        sim = arr g >>> arr h
        sim' = arr (h . g)
        tMax = maxTime msgs
        xs = runSimulator tMax sim msgs
        ys = runSimulator tMax sim' msgs


arrowLaws :: TestTree
arrowLaws = testGroup "Arrow Laws"
    [ testProperty "Arrow identity test (arr id == id)" arrowIdentity
    , testProperty "Category identity test" categoryIdentity
    , testProperty "Arrow distributive test (arr (h . g) == arr g >>> arr h)" arrowDistributiveTest]
