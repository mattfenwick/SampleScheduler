module Selector (
	randomPoints,
	dimProb,
	dimProbsInd
) where

import Model
import qualified Data.Map as M
import qualified Random as R
import qualified Data.List as L
import qualified Data.Ord as O


-- randomly select n points
--	what if the schedule has fewer than n points?            current answer: then all points are selected
--	are transients of a point selected individually?         current answer: no, this selects (Point, transient) pairs as a group
--	are the Int's in the type signature bad?                 current answer: leave them for now, refactor to Integral/Integer if necessary later
randomPoints :: Int -> Int -> Schedule -> Schedule
randomPoints number seed (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = map fst $ selectBest number snd ratedPoints
    ratedPoints = zip (M.toList pts) randomNums
    randomNums = R.randoms (R.mkStdGen seed) :: [Double]


-- select n points, using a weighting function
--	what if the schedule has fewer than n points?
--	the weighting function only takes the GridPoint into account, not the QuadUnit (nor the transients)
--	transients are not selected individually
--	the weighting function returns an Integer; would a floating-point number be better?
--	after weighting, the n points with the highest weights are selected -- this is not probabilistic
dimProb :: (GridPoint -> Integer) -> Int -> Schedule -> Schedule
dimProb f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = selectBest number (f . gridPoint . fst) $ M.toList pts


dimProbsInd :: (Integer -> Double) -> Int -> Schedule -> Schedule
dimProbsInd f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = selectBest number (product . map f . gridPoint . fst) $ M.toList pts


selectBest :: (Ord b) => Int -> (a -> b) -> [a] -> [a]
selectBest num f = take num . reverse . L.sortBy (O.comparing f)


--probabilitySelection :: Int -> (a -> Integer) [a] -> [a]
--probabilitySelection num f orig = 
-- create bins (one for each possible gridpoint in the schedule??)
-- determine widths for each bin, based on evaluation of probability distribution function
-- select a number (from 0 to the sum of widths of bins)
-- that number falls in a bin
-- remove that bin, place its associated point in list of ones that we're keeping
-- until we don't have to pick any more points
