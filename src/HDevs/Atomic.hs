module HDevs.Atomic
( runSimulator
, simulator
, Message
, Model
, Simulator (..)
) where

import qualified Control.Category
import Control.Arrow
import Control.Monad.State

type Time = Double
type Name = String
type GUID = String

infinity = (1 / 0) :: Double

type Message value = (value,Time)

type ElapsedTime    = Time
type TransitionTime = Time
type LastEventTime  = Time
type NextEventTime  = Time

data Model input output
    = Atomic
        { deltaInt :: Model input output
        , deltaExt :: ElapsedTime -> input -> Model input output
        , ta       :: TransitionTime
        , lambda   :: Maybe output }


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
            
compose :: 
    Model input intermediate -> LastEventTime -> NextEventTime ->
    Model intermediate output -> LastEventTime -> NextEventTime ->
    Model input output
compose model1 tL1 tN1 model2 tL2 tN2 = Atomic deltaIntComposed deltaExtComposed taComposed lambdaComposed where

    tL = max tL1 tL2
    tN = min tN1 tN2

    deltaIntComposed
        | tN1 < tN2 =
            let 
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tL1 + ta model1
                model2' = case lambda model1 of
                    Nothing -> model2
                    Just y1 -> deltaExt model2 (tN1 - tL2) y1
                tL2' = tN1
                tN2' = tL2' + ta model2'
            in
                compose model1' tL1' tN1' model2' tL2' tN2'

        | tN1 > tN2 =
            let
                model2' = deltaInt model2
                tL2' = tN2
                tN2' = tL2' + ta model2'
            in
                compose model1 tL1 tN1 model2' tL2' tN2'

        | tN1 == tN2 =
            let 
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tL1' + ta model1'
                model2' = deltaInt model2
                model2'' = case lambda model1 of
                    Nothing -> model2'
                    Just y1 -> deltaExt model2' 0 y1
                tL2'' = tN2
                tN2'' = tL2'' + ta model2''
            in
                compose model1' tL1' tN1' model2'' tL2'' tN2''

    deltaExtComposed e x = compose model1' tL1' tN1' model2 tL2 tN2
        where
            model1' = deltaExt model1 e1 x
            e1 = if tL1 > tL2 then e else e + (tL2 - tL1)
            tL1' = tL + e
            tN1' = tL1' + ta model1'

    taComposed = min (tN1 - tL) (tN2 - tL)

    lambdaComposed
        | tN2 <= tN1 = lambda model2
        | otherwise  = Nothing


instance Arrow Simulator where
    arr f = simulator (staticFunction f)
    first = error "first undefined"


runSimulator :: Time -> Simulator input output -> [Message input] -> [Message output]

runSimulator tMax sim@(Simulator tL tN model) msgs@((x,t):msgs')
    | tN > tMax && t > tMax = []
    | (tN <= tMax || t <= tMax) && tN < t =
        let
            (yMsg,sim') = internalTransition sim
            yMsgs = runSimulator tMax sim' msgs
        in
            case yMsg of
                Nothing -> yMsgs
                Just y  -> (y,tN): yMsgs
    | otherwise =
        runSimulator tMax (externalTransition (x,t) sim) msgs'

runSimulator tMax sim@(Simulator tL tN model) []
    | tMax < tN = []
    | otherwise =
        let 
            (yMsg,sim') = internalTransition sim
            yMsgs = runSimulator tMax sim' []
        in
            case yMsg of
                Nothing -> yMsgs
                Just y  -> (y,tN): yMsgs


internalTransition :: Simulator input output -> (Maybe output,Simulator input output)

internalTransition (Simulator tL tN model) =
    (yMsg,Simulator tL' tN' model')
        where
            model' = deltaInt model
            tL' = tN
            tN' = tL' + ta model'
            yMsg = lambda model


externalTransition :: Message input -> Simulator input output -> Simulator input output
externalTransition (x,t) (Simulator tL tN model) =
    Simulator tL' tN' model'
        where
            e = tN - tL
            model' = deltaExt model e x
            tL' = t
            tN' = tL' + ta model'


staticFunction :: (input -> output) -> Model input output
staticFunction f = wait where

    wait = Atomic (error "deltaInt for wait undefined") deltaExt' infinity Nothing

    send y = Atomic wait deltaExt' 0 (Just y)

    deltaExt' _ x = send (f x)


