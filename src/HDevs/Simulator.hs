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
, module Control.Category
, lift
, first
, (***)
, (>>>)
, (<<<)
) where

import HDevs.Atomic

import qualified Control.Category
import Data.These


-- | Represention of a simulation progress.
data Simulator input output =
    Simulator LastEventTime NextEventTime (Model input output)

-- | Building element for streams of events.
type Message value = (value,Time)


-- | Creates a simulator with a given model. 
simulator :: Model input output -> Simulator input output
simulator model = Simulator 0 (ta model) model


-- |
--     --------     --------
-- --> |model1| --> |model2| -->
--     --------     --------
--
-- composition = model1 >>> model2 = compose model1 model2 = model2 . model1
--
instance Control.Category.Category Simulator where

    id = lift id

    (.) = (<<<)            


(>>>) :: Simulator input signal -> Simulator signal output -> Simulator input output
(>>>) = flip (<<<)


(<<<) :: Simulator signal output -> Simulator input signal -> Simulator input output
(Simulator tL2 tN2 model2) <<< (Simulator tL1 tN1 model1) =
    Simulator tL tN model where
        tL = max tL1 tL2
        tN = min tN1 tN2

        model = compose model1 tL1 tN1 model2 tL2 tN2


lift :: (input -> output) -> Simulator input output
lift = simulator . static


(***) ::
    Simulator input1 output1 ->
    Simulator input2 output2 ->
    Simulator (These input1 input2) (These output1 output2)

(Simulator tL1 tN1 model1) *** (Simulator tL2 tN2 model2) =
    Simulator tL tN model where
        tL = max tL1 tL2
        tN = min tN1 tN2

        model = parallel model1 tL1 tN1 model2 tL2 tN2



first :: Simulator input output -> Simulator (These input signal) (These output signal)
first simulator = simulator *** Control.Category.id


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

