module HDevs.Data where

type Time = Double

forever :: Time
forever = 1 / 0

type Message value = (value,Time)

type ElapsedTime    = Time
type TransitionTime = Time
type LastEventTime  = Time
type NextEventTime  = Time


