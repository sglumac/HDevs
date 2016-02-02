module Test.Utility
( messagesApproxEqual
, assertMessagesApproxEqual
, sortMsgs
, maxTime
, approxEqual )
where

import HDevs
import HDevs.Data

import Data.Function (on)
import Data.List (sortBy)

import Test.Tasty.HUnit

messagesApproxEqual :: (Num signal, Ord signal, Fractional signal) =>
    [Message signal] -> [Message signal] -> Bool

messagesApproxEqual xs ys
    = length xs == length ys && allApproxEqual
    where
        msgApproxEqual (x,tx) (y,ty) = approxEqual 1e-6 x y && approxEqual 1e-6 tx ty
        allApproxEqual = all id $ zipWith msgApproxEqual xs ys


assertMessagesApproxEqual :: (Num signal, Ord signal, Fractional signal) =>
    [Message signal] -> [Message signal] -> Assertion

assertMessagesApproxEqual xs ys =
    assertBool "Message streams are not even approximately equal!" (messagesApproxEqual xs ys)


sortMsgs :: [Message input] -> [Message input]
sortMsgs = sortBy (compare `on` snd)


approxEqual :: (Num a, Ord a) =>
    a -> a -> a -> Bool

approxEqual eps x1 x2 = abs (x1 - x2) < eps


assertApproxEqual :: (Num a, Ord a, Show a) =>
    a -> a -> a -> Assertion

assertApproxEqual eps x1 x2 = assertBool msg (approxEqual eps x1 x2) where
    msg = "Element " ++ show x1 ++ " is not approximatelly equal to " ++ show x2


maxTime :: [Message signal] -> Time
maxTime [] = 0.0
maxTime xs = maximum $ map snd xs

