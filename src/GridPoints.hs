module GridPoints (
	uniformGrid,
	allLowerBounds,
	firstPoint,
	lastPoint
) where

import Model



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
    onLowerEdge pt = any (\((l, _), c) -> l == c) (zip bounds pt)    -- can this be refactored (to use zipWith or as a ZipList?)
                                                                     --           or maybe use the 'Any' monoid (LYAH Ch 11)

firstPoint :: (Integral t) => [(t, t)] -> [[t]]
firstPoint bounds = [map fst bounds]

lastPoint :: (Integral t) => [(t, t)] -> [[t]]
lastPoint bounds = [map snd bounds]

--concentricShell :: (Integral t, Num t1) => [(t,t)] -> t1 -> t1 [[t]]
--concentricShell bounds spacing maxdev = filter close $ uniformGrid bounds
--  where
--    close coord = mydist <= maxdev
--    mydist = abs (ratio - (floor ratio)) * spacing
--   ratio = 