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
        , deltaCon :: input -> Model input output
        , ta       :: TransitionTime
        , lambda   :: Maybe output }


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
    Atomic noInternal transition noConfluent forever Nothing
    where
        noInternal = error "Passive state has no internal transitions!"
        noConfluent = error "Passive state has no confluent transitions!"


hold ::
    Time -> Model input output -> (ElapsedTime -> input -> Model input output) ->
    output -> Model input output

hold t next transition out = Atomic next transition noConfluent t (Just out)
    where
        noConfluent = error "Confluent transition not defined!"
