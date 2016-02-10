module HDevs.Atomic
( Model (..)
, Message
, Time
, LastEventTime
, NextEventTime
, static
, accumulator
, passive
, hold
, forever
) where

-- | Basic DEVS model 
data Model input output
    = Atomic
        { deltaInt :: Model input output
        , deltaExt :: ExternalTransition input output
        , deltaCon :: ConfluentTransition input output
        , ta       :: TransitionTime
        , lambda   :: Maybe output }

type ExternalTransition input output = ElapsedTime -> input -> Model input output
type ConfluentTransition input output = input -> Model input output
type Time = Double

forever :: Time
forever = read "Infinity"

type Message value = (value,Time)

type ElapsedTime    = Time
type TransitionTime = Time
type LastEventTime  = Time
type NextEventTime  = Time

static:: (input -> output) -> Model input output

static f =  wait where

    wait = passive transition

    transition _ x = transition' x

    transition' x = hold 0 wait transition transition' (f x)


accumulator ::
    (input -> state -> state) -> (state -> output) ->
    state -> Model input output

accumulator f g s0 = wait where

    wait = passive transition

    next x = accumulator f g (f x s0)

    transition _ x = transition' x

    transition' x = hold 0 (next x) transition transition' (g s0)


passive :: ExternalTransition input output -> Model input output

passive transition =
    Atomic noInternal transition noConfluent forever Nothing
    where
        noInternal = error "Passive state has no internal transitions!"
        noConfluent = error "Passive state has no confluent transitions!"


hold ::
    Time -> Model input output -> ExternalTransition input output ->
    ConfluentTransition input output -> output -> Model input output

hold t next transition confluent out = Atomic next transition confluent t (Just out)

