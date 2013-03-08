module GridPoints (

    uniformGrid
  , allLowerBounds
  , firstPoint
  , lastPoint
    
) where

import Model


uniformGrid :: (Enum t) => [(t, t)] -> [[t]]
uniformGrid = sequence . map (uncurry enumFromTo)
    

-- given two "points", do any of the corresponding "fields" match?
-- i.e. [1, 2, 4] matches [3, 2, 1], but not [4, 1, 2]
isAligned :: Eq t => [t] -> [t] -> Bool
isAligned lows = or . zipWith (==) lows


aligned :: Eq t => [t] -> [[t]] -> [[t]]
aligned = filter . isAligned


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

