module HDevs
( runSimulator
, simulator
, Message
, Model
, Simulator
, module Control.Arrow
) where

import HDevs.Atomic
import HDevs.Data

import qualified Control.Category
import Control.Arrow


data Simulator input output =
    Simulator LastEventTime NextEventTime (Model input output)


simulator :: Model input output -> Simulator input output
simulator model =  Simulator 0 (ta model) model

--
--     --------     --------
-- --> |model1| --> |model2| -->
--     --------     --------
--
-- composition = model1 >>> model2 = compose model1 model2 = model2 . model1
--
instance Control.Category.Category Simulator where

    id = arr id

    (Simulator tL2 tN2 model2) . (Simulator tL1 tN1 model1) =
        Simulator tL tN model where
            tL = max tL1 tL2
            tN = min tN1 tN2

            model = compose model1 tL1 tN1 model2 tL2 tN2
            

instance Arrow Simulator where
    arr f = simulator (static f)
    first = error "first undefined"


runSimulator :: Time -> Simulator input output -> [Message input] -> [Message output]

runSimulator tMax sim@(Simulator tL tN model) msgs@((x,t):msgs')
    | tN > tMax && t > tMax = []
    | tN <= tMax && tN < t =
        let
            (yMsg,sim') = internal sim
            yMsgs = runSimulator tMax sim' msgs
        in
            case yMsg of
                Nothing -> yMsgs
                Just y  -> (y,tN): yMsgs
    | otherwise =
        runSimulator tMax (external (x,t) sim) msgs'

runSimulator tMax sim@(Simulator tL tN model) []
    | tMax < tN = []
    | otherwise =
        let 
            (yMsg,sim') = internal sim
            yMsgs = runSimulator tMax sim' []
        in
            case yMsg of
                Nothing -> yMsgs
                Just y  -> (y,tN): yMsgs


internal :: Simulator input output -> (Maybe output,Simulator input output)

internal (Simulator tL tN model) =
    (yMsg,Simulator tL' tN' model')
        where
            model' = deltaInt model
            tL' = tN
            tN' = tL' + ta model'
            yMsg = lambda model


external :: Message input -> Simulator input output -> Simulator input output
external (x,t) (Simulator tL tN model) =
    Simulator tL' tN' model'
        where
            e = tN - tL
            model' = deltaExt model e x
            tL' = t
            tN' = tL' + ta model'

