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


-- perform selection with replacement?
-- 	issue with replace:  what should be done with the transients?  add them? ignore them?
-- what if the schedule has fewer than n points?  	current answer:  then stuff is just selected multiple times
probabilitySelection :: Int -> Int -> (a -> Double) -> [a] -> [a]
probabilitySelection seed num f orig = pickedPts
  where
    binWidths = map f orig
-- determine widths for each bin, based on evaluation of probability distribution function
    relBinWidths = map (\x -> x / (sum binWidths)) binWidths
-- scale the bin widths so that their sum is 1
    (_, bins) = fmap reverse $ foldl (\(sm, bs) w -> (sm + w, (sm + w) : bs)) (0, []) relBinWidths -- :: (Num t) => (t, [t])
-- create bins (one for each possible gridpoint in the schedule??), where each bin is an endpoint of an interval (and the startpoint is the previous bin)
    binPoints = zip bins orig
-- associate a pt (or an 'a' in the type signature) with each bin
    randNums = R.randoms $ R.mkStdGen seed :: [Double]
-- need random numbers between 0 and 1 to select the bins
    pickedPts = map (pickPoint binPoints) $ take num randNums
-- use the random numbers to pick the points
    pickPoint ((p, pt): rest) r
      | r <= p = pt
      | r <= 1 = pickPoint rest r
    pickPoint [] _ = error "pickedPts was misprogrammed, the last probability should always be one, but somehow something failed" 
-- if the random number is less than the sum, pick that point
-- otherwise, continue with the next (sum, point) pair

