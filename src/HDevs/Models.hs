module HDevs.Models where

import HDevs.MessageStream
import HDevs.Atomic

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


-- |
-- Atomic DEVS model which accumulates inputs in a state.
-- This accumulator is dynamic since it takes in the elapsed time in the
-- previous state. It is useful for building dynamic models, e.g. integrators.
dynamicAccumulator ::
    (input -> ElapsedTime -> state -> state) -> (state -> output) ->
    state -> Model input output

dynamicAccumulator f g s0 = wait where

    wait = passive transition

    transition e x =
        let
            s = f x e s0
            y = g s
            next = dynamicAccumulator f g s
        in
            hold 0 next transition (transition 0) y


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


-- | Simple integrator using Euler integration (assumes step sequence as a signal)
integrator :: Double -> Model Double Double

integrator s0 = dynamicAccumulator euler fst (s0,0)
    where
        euler x e (s,ts) = (s + x * e, ts + e)


-- | Proportional time delay model.
pt1 :: Double -> Time -> Double -> Model Double Double
pt1 gain timeConstant s0 = dynamicAccumulator calculation fst (s0,0)
    where
        calculation x e (s,ts) =
            let
                s' = timeConstant * s / (timeConstant + e) + e * gain * x / (timeConstant + e)
                ts' = ts + e
            in
                (s',ts')


-- | Counts the number of events.
count :: Model input Int
count = accumulator (\_ counter -> counter +1) id 0

