module Test.Utility
( gainSimulator
, outputsEqual
, sameOutputs
, sortMsgs )
where

import HDevs

import Data.Function (on)
import Data.List (sortBy)


gainSimulator :: Double -> Simulator Double Double
gainSimulator k = arr (k*)


outputsEqual :: Eq output =>
    [Message input] -> Simulator input output -> [Message output] -> Bool

outputsEqual [] sim outMsgs = runSimulator 1.0 sim [] == outMsgs

outputsEqual inMsgs sim outMsgs = runSimulator tMax sim inMsgs == outMsgs
    where
        tMax = maximum . map snd $ inMsgs


sameOutputs :: Eq output =>
    [Message input] -> Simulator input output -> Simulator input output -> Bool

sameOutputs [] sim1 sim2 = outMsgs1 == outMsgs2
    where
        outMsgs1 = runSimulator 1.0 sim1 []
        outMsgs2 = runSimulator 1.0 sim2 []

sameOutputs inMsgs sim1 sim2 = outMsgs1 == outMsgs2
    where
        outMsgs1 = runSimulator tMax sim1 inMsgs
        outMsgs2 = runSimulator tMax sim2 inMsgs
        tMax = maximum . map snd $ inMsgs


sortMsgs :: [Message input] -> [Message input]
sortMsgs = sortBy (compare `on` snd)
