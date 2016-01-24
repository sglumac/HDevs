module ArrowLaws (arrowLawsGroup) where

import HDevs.Atomic
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Control.Arrow

import Data.Function (on)
import Data.List (sortBy)


arrowIdentityCheck :: [Message Double] -> Bool
arrowIdentityCheck msgs = runSimulator tMax idSim msgs' == msgs'
    where
        msgs' = sortBy (compare `on` snd) msgs
        tMax = if msgs == [] then 1.0 else maximum . map snd $ msgs
        idSim = arr id

arrowLawsGroup :: TestTree
arrowLawsGroup = testGroup "Arrow Laws"
    [ testProperty "Arrow identity test (arr id == id):" arrowIdentityCheck ]
