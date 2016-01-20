module HDevs.Examples
( gainExperiment
, gains2Experiment)
where

import HDevs.Atomic
import Control.Arrow
import Control.Monad.State.Lazy


--gainModel :: Double -> Model Double Double
--gainModel multiplier = arr (multiplier*)

gainExperiment :: [Message Double]
gainExperiment = runSimulator 10.0 sim [(1.0,1.0),(2.0,2.0),(6.0,3.0)]
    where sim = arr (4*) --simulator (gainModel 4)

gains2Experiment = runSimulator 10.0 (sim4 >>> sim5) [(1.0,1.0)] --,(2.0,2.0),(6.0,3.0)]
    where
        sim4 = arr (4*) --simulator (gainModel 4)
        sim5 = arr (5*) --simulator (gainModel 5)
