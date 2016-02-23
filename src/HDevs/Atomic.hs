-----------------------------------------------------------------------------
-- |
-- Module      :  HDevs.Atomic
-- Copyright   :  (c) Slaven Glumac 2016
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  slaven.glumac@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Specification of atomic DEVS model (https://en.wikipedia.org/wiki/DEVS#Atomic_DEVS).
--

module HDevs.Atomic where

import Data.These

-- | Basic DEVS model 
data Model input output
    = Atomic
        { deltaInt :: Model input output
        , deltaExt :: ExternalTransition input output
        , deltaCon :: ConfluentTransition input output
        , ta       :: TransitionTime
        , lambda   :: Maybe output }


-- | Model function used for reaction to external input.
type ExternalTransition input output = ElapsedTime -> input -> Model input output
-- | Model function used for resolving the simultaneous occurence of an external and an internal event.
type ConfluentTransition input output = input -> Model input output

-- | Time definition.
type Time = Double

-- | Elapsed time since the last event.
type ElapsedTime    = Time
-- | Time until the next internal transition.
type TransitionTime = Time
-- | Time of the last input or internal model event.
type LastEventTime  = Time
-- | Time of the next internal model event.
type NextEventTime  = Time


-- | Until the end of time.
forever :: Time
forever = read "Infinity"


-- | Lift a static function to the atomic DEVS model.
static :: (input -> output) -> Model input output

static f =  wait where

    wait = passive transition

    transition _ x = transition' x

    transition' x = hold 0 wait transition transition' (f x)


-- | Atomic DEVS model which accumulates inputs in a state (similar to fold). 
accumulator ::
    (input -> state -> state) -> (state -> output) ->
    state -> Model input output

accumulator f g s0 = wait where

    wait = passive transition

    next x = accumulator f g (f x s0)

    transition _ x = transition' x

    transition' x = hold 0 (next x) transition transition' (g s0)


-- | Atomic model which waits forever for an external input.
passive :: ExternalTransition input output -> Model input output

passive transition =
    Atomic noInternal transition noConfluent forever Nothing
    where
        noInternal = error "Passive state has no internal transitions!"
        noConfluent = error "Passive state has no confluent transitions!"


-- | Atomic model which holds the current state for a given ammount of time and transition to the next state.
hold ::
    Time -> Model input output -> ExternalTransition input output ->
    ConfluentTransition input output -> output -> Model input output

hold t next transition confluent out = Atomic next transition confluent t (Just out)


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

    deltaCon' x
        | tN1 < tN2 =
            let 
                model1' = deltaCon model1 x
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
                model1' = deltaCon model1 x
                tL1' = tN1
                tN1' = tN1 + ta model1
                model2' = case lambda model1 of
                    Nothing -> deltaInt model2
                    Just y1 -> deltaCon model2 y1
                tL2' = tN1
                tN2' = tN1 + ta model2'
            in
                compose model1' tL1' tN1' model2' tL2' tN2'

        | otherwise = error "this is hard to define since model1 outputs something and immediately changes model2"


    ta' = min (tN1 - tL) (tN2 - tL)


    lambda'
        | tN2 <= tN1 = lambda model2
        | otherwise  = Nothing


parallel :: 
    Model input1 output1 -> LastEventTime -> NextEventTime ->
    Model input2 output2 -> LastEventTime -> NextEventTime ->
    Model (These input1 input2) (These output1 output2)

parallel model1 tL1 tN1 model2 tL2 tN2 = Atomic deltaInt' deltaExt' deltaCon' ta' lambda' where

    tL = max tL1 tL2

    deltaInt'
        | tN1 < tN2 =
            let 
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model1'
            in
                parallel model1' tL1' tN1' model2 tL2 tN2

        | tN1 == tN2 =
            let
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model1
                model2' = deltaInt model2
                tL2' = tN1
                tN2' = tN1 + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

        | otherwise =
            let
                model2' = deltaInt model2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                parallel model1 tL1 tN1 model2' tL2' tN2'

    deltaExt' e (This x1) = parallel model1' tL1' tN1' model2 tL2 tN2
        where
            model1' = deltaExt model1 e1 x1
            e1 = if tL1 > tL2 then e else e + (tL2 - tL1)
            tL1' = tL + e1
            tN1' = tL1' + ta model1'
    deltaExt' e (That x2) = parallel model1 tL1 tN1 model2' tL2' tN2'
        where
            model2' = deltaExt model2 e2 x2
            e2 = if tL2 > tL1 then e else e + (tL1 - tL2)
            tL2' = tL + e2
            tN2' = tL2' + ta model2'
    deltaExt' e (These x1 x2) = parallel model1' tL1' tN1' model2' tL2' tN2'
        where
            model1' = deltaExt model1 e1 x1
            e1 = if tL1 > tL2 then e else e + (tL2 - tL1)
            tL1' = tL + e1
            tN1' = tL1' + ta model1'
            model2' = deltaExt model2 e2 x2
            e2 = if tL2 > tL1 then e else e + (tL1 - tL2)
            tL2' = tL + e2
            tN2' = tL2' + ta model2'

    deltaCon' (This x1)
        | tN1 < tN2 =
            -- confluent model1, no transition model2
            let 
                model1' = deltaCon model1 x1
                tL1' = tN1
                tN1' = tN1 + ta model1'
            in
                parallel model1' tL1' tN1' model2 tL2 tN2

        | tN1 == tN2 =
            -- confluent model1, internal model2
            let
                model1' = deltaCon model1 x1 
                tL1' = tN1
                tN1' = tN1 + ta model1'
                model2' = deltaInt model2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

        | otherwise =
            -- external model1, internal model2
            let
                model1' = deltaExt model1 e1 x1
                e1 = tN2 - tL1
                tL1' = tL + e1
                tN1' = tL1' + ta model1'
                model2' = deltaInt model2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

    deltaCon' (That x2)
        | tN2 < tN1 =
            -- no transition model1, confluent model2
            let 
                model2' = deltaCon model2 x2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                parallel model1 tL1 tN1 model2' tL2' tN2'

        | tN1 == tN2 =
            -- internal model1, confluent model2
            let
                model2' = deltaCon model2 x2
                tL2' = tN2
                tN2' = tN2 + ta model2
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

        | otherwise =
            -- internal model1, external model2
            let
                model2' = deltaExt model2 e2 x2
                e2 = tN1 - tL2
                tL2' = tL + e2
                tN2' = tL2' + ta model2'
                model1' = deltaInt model1
                tL1' = tN1
                tN1' = tN1 + ta model1'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

    deltaCon' (These x1 x2)
        | tN1 < tN2 =
            -- confluent model1, external model2
            let 
                model1' = deltaCon model1 x1
                tL1' = tN1
                tN1' = tN1 + ta model1'
                model2' = deltaExt model2 e2 x2
                e2 = tN1 - tL2
                tL2' = tL + e2
                tN2' = tL2' + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

        | tN1 == tN2 =
            -- confluent model1, confluent model2
            let
                model1' = deltaCon model1 x1 
                tL1' = tN1
                tN1' = tN1 + ta model1'
                model2' = deltaCon model2 x2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

        | otherwise =
            -- external model1, confluent model2
            let
                model1' = deltaExt model1 e1 x1
                e1 = tN2 - tL1
                tL1' = tL + e1
                tN1' = tL1' + ta model1'
                model2' = deltaCon model2 x2
                tL2' = tN2
                tN2' = tN2 + ta model2'
            in
                parallel model1' tL1' tN1' model2' tL2' tN2'

    ta' = min (tN1 - tL) (tN2 - tL)

    lambda'
        | tN1 < tN2 = This `fmap` (lambda model1)
        | tN2 < tN1 = That `fmap` (lambda model2)
        | otherwise =
            let
                merge (Just y1) (Just y2) = Just $ These y1 y2
                merge (Just y1) Nothing = Just $ This y1
                merge Nothing (Just y2) = Just $ That y2
                merge Nothing Nothing = Nothing
            in
                merge (lambda model1) (lambda model2)

