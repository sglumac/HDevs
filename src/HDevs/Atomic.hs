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


