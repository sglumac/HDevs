module Test.Atomic where

import HDevs.Atomic
import HDevs.Models

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

noWaitOutputIntegrator :: Assertion
noWaitOutputIntegrator = lambda (integrator 0) @?= Nothing

sendOutputIntegrator :: Assertion
sendOutputIntegrator = assertBool msg $ correct (lambda integrator')
    where
        integrator' = deltaExt (integrator 0) 3 4
        correct Nothing = False
        correct (Just y) = abs (y - 12) < 1e-6
        msg = "The result was " ++ show (lambda integrator') ++ " instead of (Just 12)"


atomicTests :: TestTree
atomicTests = testGroup "Atomic Tests"
    [ testCase "Gain model should wait forever" foreverWait
    , testCase "Gain should output Nothing while waiting" noWaitOutput
    , testCase "Gain should output in send immediatelly" immediateSend
    , testCase "Gain should output correctly in send state" sendOutput
    , testCase "Integrator should output Nothing while waiting" noWaitOutputIntegrator
    , testCase "Integrator should output correctly in send state" sendOutputIntegrator ]

