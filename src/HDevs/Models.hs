module HDevs.Models where

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


