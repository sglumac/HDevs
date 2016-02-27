module Test.Simulator (simulatorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Utility

import HDevs.Simulator

simpleGain :: Assertion
simpleGain = assertMessageStreamsEqual 0.1 ys ys'
    where
        sim :: Simulator Int Int
        sim = lift (4*)
        ys = runSimulator 10.0 sim [(5,1.0),(6,2.0)]
        ys' = [(20,1.0),(24,2.0)]

connectedGains :: Assertion 
connectedGains = assertMessageStreamsEqual 1e-6 ys ys'
    where
        sim :: Simulator Int Int
        sim = lift (3*) >>> lift (4*)
        ys = runSimulator 10.0 sim [(5,1.0)]
        ys' = [(60,1.0)]

connectedGains' :: Assertion
connectedGains' = assertMessageStreamsEqual 1e-6 ys ys'
    where
        sim :: Simulator Int Int
        sim = lift (3*) >>> lift (16*)
        ys = runSimulator 10.0 sim [(7,1.0),(3,2.0)]
        ys' = [(336,1.0),(144,2.0)]


simulationToShort :: Assertion
simulationToShort = assertMessageStreamsEqual 1e-6 ys ys'
    where
        sim :: Simulator Int Int
        sim = lift (4*)
        ys = runSimulator 0.1 sim [(5,1.0),(6,2.0)]
        ys' = []

type ParallelInt = These Int Int

parallelGains :: Assertion
parallelGains = assertMessageStreamsEqual 1e-6 ys ys'
    where
        sim :: Simulator  ParallelInt ParallelInt
        sim = lift (3*) *** lift (4*)
        xs = [(This 1,1.0),(That 2,2.0),(These 3 4,3.0)]
        ys = runSimulator 10.0 sim xs
        ys' = [(This 3,1.0),(That 8,2.0),(These 9 16,3.0)]


firstGain :: Assertion
firstGain = assertMessageStreamsEqual 1e-6 ys ys'
    where
        sim :: Simulator  ParallelInt ParallelInt
        sim = first $ lift (3*)
        xs = [(This 1,1.0),(That 2,2.0),(These 3 4,3.0)]
        ys = runSimulator 10.0 sim xs
        ys' = [(This 3,1.0),(That 2,2.0),(These 9 4,3.0)]


confluentTransition :: Assertion
confluentTransition = assertMessageStreamsEqual 1e-6 ys ys'
    where
        sim :: Simulator Int Int
        sim = lift (4*)
        ys = runSimulator 10.0 sim [(5,1.0),(6,1.0)]
        ys' = [(20,1.0),(24,1.0)]

simulatorTests :: TestTree
simulatorTests = testGroup "Simulator Tests"
    [ testCase "Check if a simple gain amplifies the input" simpleGain
    , testCase "Check if connected gains amplify the input" connectedGains
    , testCase "Check if connected gains amplify the input" connectedGains'
    , testCase "Check if parallel gains affect their corresponding inputs" parallelGains
    , testCase "Check if gain expanded with first works correctly" firstGain
    , testCase "Check if simulation will properly given short simulation time" simulationToShort
    , testCase "Check if simultaneous events are handled properly with confluent transitions" confluentTransition ]

