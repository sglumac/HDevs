module Test.Utility
( messagesEqual
, messageStreamsEqual
, assertMessagesEqual
, assertMessageStreamsEqual
, maxTime )
where

import HDevs.Atomic
import HDevs.Simulator hiding (id)

import Test.Tasty.HUnit

messageStreamsEqual :: Eq signal =>
    Time -> [Message signal] -> [Message signal] -> Bool

messageStreamsEqual teps msgs1 msgs2 =
    length msgs1 == length msgs2 && allMessagesEqual
    where
        allMessagesEqual = all id $ zipWith (messagesEqual teps) msgs1 msgs2


assertMessageStreamsEqual :: Eq signal =>
    Time -> [Message signal] -> [Message signal] -> Assertion

assertMessageStreamsEqual teps xs ys =
    assertBool "Message streams are not even approximately equal!" (messageStreamsEqual teps xs ys)


messagesEqual :: Eq signal =>
    Time -> Message signal -> Message signal -> Bool

messagesEqual teps (x1,t1) (x2,t2) = x1 == x2 && abs (t1 - t2) < teps


assertMessagesEqual ::  (Eq signal,Show signal) =>
    Time -> Message signal -> Message signal -> Assertion

assertMessagesEqual teps msg1 msg2 = assertBool msg (messagesEqual teps msg1 msg2) where
    msg = "Element " ++ show msg1 ++ " is not approximatelly equal to " ++ show msg2


maxTime :: [Message signal] -> Time
maxTime [] = 0.0
maxTime xs = maximum $ map snd xs

