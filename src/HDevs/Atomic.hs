module HDevs.Atomic where

import Prelude hiding (id, (.))
import Control.Monad.State.Lazy
import Control.Arrow
import Data.Maybe (maybeToList)

type Time = Double

infinity = (1 / 0) :: Double

type Message value = (value,Time)

type ElapsedTime = Time
type TransitionTime = Time

data Atomic input output = Atomic
    { deltaInt :: Atomic input output
    , deltaExt :: ElapsedTime -> input -> Atomic input output
    , ta       :: TransitionTime
    , lambda   :: Maybe output }


simulator :: Time -> [Message input] -> State ((Atomic input output),Time,Time) [Message output]
simulator tMax msgs@((x,t):msgs') = do
    (model,tL,tN) <- get
    if tN < tMax
        then
            let model' = deltaInt model
                tL' = tN
                tN' = tL' + ta model'
                yMsg = maybeToList $ (\y -> (y,tN)) `fmap` lambda model
            in do
                put (model',tL',tN')
                yMsgs <- simulator tMax msgs
                return $ yMsg ++ yMsgs
        else
            let e = t - tL
                model' = deltaExt model e x
                tL' = t
                tN' = tL' + ta model'
            in put (model',tL',tN') >> simulator tMax msgs'

simulator tMax [] = do
    (model,tL,tN) <- get
    if tN < tMax
        then
            let model' = deltaInt model
                tL' = tN
                tN' = tL' + ta model'
                yMsg = maybeToList $ (\y -> (y,tN)) `fmap` lambda model
            in do
                put (model',tL',tN')
                yMsgs <- simulator tMax []
                return $ yMsg ++ yMsgs
        else
            return []


simulate :: (Show input, Show output) =>
    Atomic input output -> Time -> [Message input] -> StateT (Time,Time) IO ()
simulate model tMax [] = do
    (tL,tN) <- get
    if tN < tMax
        then
            let model' = deltaInt model
                tL' = tN
                tN' = tL' + ta model'
                y = lambda model
            in do
                liftIO (putStrLn "Internal transition...")
                liftIO $ print (y,tL')
                put (tL',tN') >> simulate model' tMax []
        else
            liftIO $ putStrLn "Ending simulation..."

simulate model tMax msgs@((x,t):msgs') = do 
    (tL,tN) <- get
    if t > tN
        then
            let model' = deltaInt model
                tL' = tN
                tN' = tL' + ta model'
                y = lambda model
            in do
                liftIO (putStrLn "Internal transition...")
                liftIO $ print (y,tL')
                put (tL',tN') >> simulate model' tMax msgs
        else
            let e = t - tL
                model' = deltaExt model e x
                tL' = t
                tN' = tL' + ta model'
            in do
                liftIO (putStrLn "External transition...")
                liftIO (print (x,t))
                put (tL',tN') >> simulate model' tMax msgs'

staticSystem :: (input -> output) -> Atomic input output
staticSystem f = Atomic
    { deltaInt = undefined
    , deltaExt = \_ x -> staticSystem' f (f x)
    , ta       = infinity
    , lambda   = Nothing
    } where
        staticSystem' f y = Atomic
            { deltaInt = staticSystem f
            , deltaExt = \_ x -> staticSystem' f (f x)
            , ta       = 0
            , lambda   = Just y }

compose :: Atomic input intermediate -> Atomic intermediate output -> Atomic input output
compose model1 model2 = Atomic
    { deltaInt = deltaInt'
    , deltaExt = deltaExt'
    , ta       = ta'
    , lambda   = lambda'
    } where
        deltaInt' = undefined
        deltaExt' = undefined
        ta' = undefined
        lambda' = undefined

compose' = undefined
