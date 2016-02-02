module HDevs.Atomic
( Model (..)
, static
, accumulator
, compose
) where

import HDevs.Data

data Model input output
    = Atomic
        { deltaInt :: Model input output
        , deltaExt :: ElapsedTime -> input -> Model input output
        , ta       :: TransitionTime
        , lambda   :: Maybe output }


compose :: 
    Model input intermediate -> LastEventTime -> NextEventTime ->
    Model intermediate output -> LastEventTime -> NextEventTime ->
    Model input output

compose model1 tL1 tN1 model2 tL2 tN2 = Atomic deltaIntComposed deltaExtComposed taComposed lambdaComposed where

    tL = max tL1 tL2

    deltaIntComposed
        | tN1 <= tN2 =
            let 
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model1
                model2' = case lambda model1 of
                    Nothing -> model2
                    Just y1 -> deltaExt model2 (tN1 - tL2) y1
                tL2' = tN1
                tN2' = tN1 + ta model2'
            in
                compose model1' tL1' tN1' model2' tL2' tN2'

        | tN1 > tN2 =
            let
                model2' = deltaInt model2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                compose model1 tL1 tN1 model2' tL2' tN2'

    deltaExtComposed e x = compose model1' tL1' tN1' model2 tL2 tN2
        where
            model1' = deltaExt model1 e1 x
            e1 = if tL1 > tL2 then e else e + (tL2 - tL1)
            tL1' = tL + e
            tN1' = tL1' + ta model1'

    taComposed = min (tN1 - tL) (tN2 - tL)

    lambdaComposed
        | tN2 < tN1 = lambda model2
        | otherwise  = Nothing


static:: (input -> output) -> Model input output

static f =  wait where

    wait = passive transition

    transition _ x = hold 0 wait transition (f x)


accumulator ::
    (input -> state -> state) -> (state -> output) ->
    state -> Model input output

accumulator f g s0 = wait where

    wait = passive transition

    next x = accumulator f g (f x s0)

    transition _ x = hold 0 (next x) transition (g s0)


passive :: (ElapsedTime -> input -> Model input output) -> Model input output

passive transition =
    Atomic (error "Passive state has no internal transitions!") transition forever Nothing 


hold ::
    Time -> Model input output -> (ElapsedTime -> input -> Model input output) ->
    output -> Model input output

hold t next transition out = Atomic next transition t (Just out)
