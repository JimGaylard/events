module Counter (allMyCounters, myLatestCounter)
  where

import Eventful (Projection(Projection), allProjections, latestProjection, projectionSeed, projectionEventHandler)

newtype Counter = Counter { unCounter :: Int }
  deriving (Show, Eq)

data CounterEvent
  = CounterIncremented Int
  | CounterDecremented Int
  | CounterReset
  deriving (Show, Eq)

handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter count) (CounterIncremented amount) = Counter (count + amount)
handleCounterEvent (Counter count) (CounterDecremented amount) = Counter (count - amount)
handleCounterEvent _ (CounterReset) = Counter 0

counterProjection :: Projection Counter CounterEvent
counterProjection =
  Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleCounterEvent
  }

myEvents :: [CounterEvent]
myEvents =
  [ CounterIncremented 3
  , CounterDecremented 1
  , CounterReset
  ]

myLatestCounter :: Counter
myLatestCounter = latestProjection counterProjection myEvents

allMyCounters :: [Counter]
allMyCounters = allProjections counterProjection myEvents
