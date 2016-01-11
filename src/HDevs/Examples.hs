module HDevs.Examples
( gainExperiment
, gainExperiment')
where


import HDevs.Atomic
import Control.Monad.State.Lazy


gainModel :: Double -> Atomic Double Double
gainModel multiplier = staticSystem (multiplier*)

gainExperiment :: IO ()
gainExperiment = evalStateT simulation (0.0,tN)
    where simulation = simulate (gainModel 4) 10.0 [(1.0,1.0),(2.0,2.0),(6.0,3.0)]
          tN = ta $ gainModel 4

gainExperiment' :: [Message Double]
gainExperiment' = evalState sim s
    where tN = ta $ gainModel 4
          s = (gainModel 4, 0.0, tN)
          sim = simulator 10.0 [(1.0,1.0),(2.0,2.0),(6.0,3.0)]
