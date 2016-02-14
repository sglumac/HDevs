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
static:: (input -> output) -> Model input output

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

