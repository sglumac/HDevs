module Test.ArrowLaws (arrowLaws) where

import HDevs

import Prelude hiding ((.),id)
import Test.Utility

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

categoryIdentity :: [Int] -> Bool
categoryIdentity values = messageStreamsEqual 1e-6 msgs ys
    where
        msgs = zip values [1.0..]
        tMax = maxTime msgs
        ys = runSimulator tMax id msgs


-- arr id = id 
arrowIdentity :: [Int] -> Bool
arrowIdentity values = messageStreamsEqual 1e-6 msgs ys
    where
        msgs = zip values [1.0..]
        tMax = maxTime msgs
        ys = runSimulator tMax (lift id) msgs


-- arr (h . g)  =  arr g >>> arr h
arrowDistributiveTest :: Int -> Int -> [Int] -> Bool
arrowDistributiveTest k1 k2 values = messageStreamsEqual 1e-6 xs ys
    where
        msgs = zip values [1.0..]
        h = (k1*)
        g = (k2*)
        sim = lift g >>> lift h
        sim' = lift (h . g)
        tMax = maxTime msgs
        xs = runSimulator tMax sim msgs
        ys = runSimulator tMax sim' msgs


arrowLaws :: TestTree
arrowLaws = testGroup "Arrow Laws"
    [ testProperty "Arrow identity test (arr id == id)" arrowIdentity
    , testProperty "Category identity test" categoryIdentity
    , testProperty "Arrow distributive test (arr (h . g) == arr g >>> arr h)" arrowDistributiveTest]

