-----------------------------------------------------------------------------
-- |
-- Module      :  HDevs.Simulator
-- Copyright   :  (c) Slaven Glumac 2016
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  slaven.glumac@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of arrowized DEVS simulator.
--

module HDevs.Simulator
( runSimulator
, simulator
, Message
, Model
, Simulator
, module Control.Arrow
) where

import HDevs.Atomic

import qualified Control.Category
import Control.Arrow


-- | Represention of a simulation progress.
data Simulator input output =
    Simulator LastEventTime NextEventTime (Model input output)

-- | Building element for streams of events.
type Message value = (value,Time)


-- | Creates a simulator with a given model. 
simulator :: Model input output -> Simulator input output
simulator model = Simulator 0 (ta model) model


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


-- | Runs the simulator for a given simulation time on the stream of inputs
runSimulator :: Time -> Simulator input output -> [Message input] -> [Message output]

runSimulator tMax sim@(Simulator _ tN _) msgs@((x,t):msgs')
    | tN > tMax && t > tMax = []
    | tN <= tMax && tN < t =
        let
            (yMsg,sim') = internal sim
            yMsgs = runSimulator tMax sim' msgs
        in
            case yMsg of
                Nothing -> yMsgs
                Just y  -> (y,tN): yMsgs
    | tN <= tMax && tN == t =
        let
            (yMsg,sim') = confluent sim x
            yMsgs = runSimulator tMax sim' msgs
        in
            case yMsg of
                Nothing -> yMsgs
                Just y  -> (y,tN): yMsgs
    | otherwise =
        runSimulator tMax (external sim (x,t)) msgs'

runSimulator tMax sim@(Simulator _ tN _) []
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

internal (Simulator _ tN model) =
    (yMsg,Simulator tL' tN' model')
        where
            model' = deltaInt model
            tL' = tN
            tN' = tL' + ta model'
            yMsg = lambda model


external :: Simulator input output -> Message input -> Simulator input output
external (Simulator tL _ model) (x,t) =
    Simulator tL' tN' model'
        where
            e = t - tL
            model' = deltaExt model e x
            tL' = t
            tN' = tL' + ta model'

confluent :: Simulator input output -> input -> (Maybe output,Simulator input output)

confluent (Simulator _ tN model) x =
    (yMsg,Simulator tL' tN' model')
        where
            model' = deltaCon model x
            tL' = tN
            tN' = tL' + ta model'
            yMsg = lambda model


compose :: 
    Model input intermediate -> LastEventTime -> NextEventTime ->
    Model intermediate output -> LastEventTime -> NextEventTime ->
    Model input output

compose model1 tL1 tN1 model2 tL2 tN2 = Atomic deltaInt' deltaExt' deltaCon' ta' lambda' where

    tL = max tL1 tL2

    deltaInt'
        | tN1 < tN2 =
            let 
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model1'
                model2' = case lambda model1 of
                    Nothing -> model2
                    Just y1 -> deltaExt model2 (tN1 - tL2) y1
                tL2' = tN1
                tN2' = tN1 + ta model2'
            in
                compose model1' tL1' tN1' model2' tL2' tN2'

        | tN1 == tN2 =
            let
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model1
                model2' = case lambda model1 of
                    Nothing -> deltaInt model2
                    Just y1 -> deltaCon model2 y1
                tL2' = tN1
                tN2' = tN1 + ta model2'
            in
                compose model1' tL1' tN1' model2' tL2' tN2'

        | otherwise =
            let
                model2' = deltaInt model2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                compose model1 tL1 tN1 model2' tL2' tN2'

    deltaExt' e x = compose model1' tL1' tN1' model2 tL2 tN2
        where
            model1' = deltaExt model1 e1 x
            e1 = if tL1 > tL2 then e else e + (tL2 - tL1)
            tL1' = tL + e1
            tN1' = tL1' + ta model1'

    deltaCon' _ = error "deltaCon' undefined"

    ta' = min (tN1 - tL) (tN2 - tL)

    lambda'
        | tN2 <= tN1 = lambda model2
        | otherwise  = Nothing
