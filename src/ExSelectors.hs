module ExSelectors (
	randomPoints,
	bestByGridPoint,
	randomPointsAllQuadUnits,
	bestByGridPointAllQuadUnits,
	probByGridPoint
) where

import Model
import Selector
import qualified Random as R




-- randomly select n points
--	are the Int's in the type signature bad?                current answer: leave them for now, refactor to Integral/Integer if necessary later
randomPoints :: Int -> Int -> Schedule -> Schedule
randomPoints n s = genericSelect combTransSepQuad (\_ -> ()) (\_ -> selectNRandomly n s)


-- select n points, using a weighting function
--	what if the schedule has fewer than n points?
--	the weighting function returns an Integer; would a floating-point number be better?
bestByGridPoint :: Int -> (GridPoint -> Integer) -> Schedule -> Schedule
bestByGridPoint n f = genericSelect combTransSepQuad f (selectNBest n)


-- should I have to specify 'Int'?  No!!!!
probByGridPoint :: (Fractional t, R.Random t, Ord t) => Int -> Int -> (GridPoint -> t) -> Schedule -> Schedule
probByGridPoint n s f = genericSelect combTransSepQuad f (selectNProb n s)


bestByGridPointAllQuadUnits :: Int -> (GridPoint -> Integer) -> Schedule -> Schedule
bestByGridPointAllQuadUnits n f = genericSelect combTransCombQuad f (selectNBest n)


randomPointsAllQuadUnits :: Int -> Int -> Schedule -> Schedule
randomPointsAllQuadUnits n s = genericSelect combTransCombQuad (\_ -> ()) (\_ -> selectNRandomly n s)