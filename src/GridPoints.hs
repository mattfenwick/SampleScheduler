module GridPoints (
	uniformGrid,
	allLowerBounds,
	firstPoint,
	lastPoint
) where

import Model
import Control.Applicative



-- randomGrid2d :: (Integer, Integer) -> (Integer, Integer) -> Integer -> [GridPoint]
-- randomGrid2d (xl, xh) (yl, yh) totalpoints = random number generator ...

uniformGrid :: (Enum t) => [(t, t)] -> [[t]]
uniformGrid bounds = sequence ranges
  where
    ranges = map (\(l, h) -> [l .. h]) bounds

allLowerBounds :: (Integral t) => [(t, t)] -> [[t]]
allLowerBounds bounds = filter onLowerEdge gridpoints
  where
    gridpoints = uniformGrid bounds
    onLowerEdge pt = any (\((l, _), c) -> l == c) (zip bounds pt)

firstPoint :: (Integral t) => [(t, t)] -> [[t]]
firstPoint bounds = [map fst bounds]

lastPoint :: (Integral t) => [(t, t)] -> [[t]]
lastPoint bounds = [map snd bounds]