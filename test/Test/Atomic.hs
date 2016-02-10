module Test.Atomic where

import HDevs.Atomic

import Test.Tasty
import Test.Tasty.HUnit

gain :: Model Double Double
gain = static (4*)

gain' :: Model Double Double
gain' = deltaExt gain 2.3 3

foreverWait :: Assertion
foreverWait = ta gain @?= forever

noWaitOutput :: Assertion
noWaitOutput = lambda gain @?= Nothing

immediateSend :: Assertion
immediateSend = ta gain' @?= 0.0

sendOutput :: Assertion
sendOutput = lambda gain' @?= Just (4 * 3)


atomicTests :: TestTree
atomicTests = testGroup "Atomic Tests"
    [ testCase "Gain model should wait forever" foreverWait
    , testCase "Gain should output Nothing while waiting" noWaitOutput
    , testCase "Gain should output in send immediatelly" immediateSend
    , testCase "Gain should output correctly in send state" sendOutput]

