module Z.GridPoints (

    uniformGrid
  , allLowerBounds
  , firstPoint
  , lastPoint

) where

import Z.Generators


uniformGrid :: Enum t => [(t, t)] -> [[t]]
uniformGrid = uniform


allLowerBounds :: (Eq t, Enum t) => [(t, t)] -> [[t]]
allLowerBounds bounds = 
    let
       lows = map fst bounds
    in
       aligned lows (uniformGrid bounds)
    

firstPoint :: [(t, t)] -> [[t]]
firstPoint = (:[]) . map fst


lastPoint :: [(t, t)] -> [[t]]
lastPoint = (:[]) . map snd