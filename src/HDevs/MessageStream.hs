module HDevs.MessageStream where

-- | Time definition.
type Time = Double

-- | Building element for input and output streams.
type Message value = (value,Time)

-- | Type modeling input and output streams.
type MessageStream value = [Message value]

