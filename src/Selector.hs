module Selector (

    genericSelect
  , selectNBest
  , selectNRandomly
  , selectWithRandomness
  , selectNProb

) where

import Model
import Grouper
import System.Random  (randoms, mkStdGen, Random)
import Data.List      (sortBy, foldl')
import Data.Ord       (comparing)



------------------------------------------------------------------------------------------------
-- selection strategies
-- selectNBest and selectNRandomly:  no replacement; if input has fewer than 'num' elements, all elements are selected

-- apply a function to each element, and select the n elements with the best scores
selectNBest :: (Ord b) => Int -> (a -> b) -> [a] -> [a]
selectNBest num f = take num . reverse . sortBy (comparing f)


selectNRandomly :: Int -> Int -> [a] -> [a]
selectNRandomly num seed things = map fst $ selectNBest num snd ratedPoints
  where
    ratedPoints = zip things randomNums
    randomNums = randoms (mkStdGen seed) :: [Double]


selectWithRandomness :: Int -> Int -> (a -> Double) -> [a] -> [a]
selectWithRandomness num seed f things = map fst $ selectNBest num snd randomRatedPoints
  where
    randomRatedPoints = map (\(r, (t, w)) -> (t, r * w)) $ zip randomNums ratedPoints
    ratedPoints = map (\t -> (t, f t)) things
    randomNums = randoms (mkStdGen seed) :: [Double]


-- perform probabilistic selection with replacement
selectNProb :: (Fractional t, Random t, Ord t) => Int -> Int -> (a -> t) -> [a] -> [a]
selectNProb _ _ _ [] = []
selectNProb num seed f orig = pickedPts
  where
    binWidths = map f orig                                      -- determine bin widths from probability distribution function
    relBinWidths = map (\x -> x / (sum binWidths)) binWidths    -- scale bin widths so that sum is 1
    (_, bins) = createBins relBinWidths                         -- create a bin for each element
                                                                --     a bin is an endpoint of an interval (the startpoint is the previous bin)
    binPoints = zip bins orig                                   -- associate an element with each bin
    randNums = randoms $ mkStdGen seed -- :: [Double]           -- generate random numbers between 0 and 1
    pickedPts = map (pickPoint binPoints) $ take num randNums   -- use 'num' random numbers to pick bins
    pickPoint ((p, pt): rest) r
      | r <= p = pt                                             -- if the random number is less than the sum, pick that point
      | r <= 1 = pickPoint rest r                               -- otherwise, continue with the next (sum, point) pair
    pickPoint [] _ = error "expected: 0 <= x <= 1; actual: x > 1"                        -- if this ever happens, the programmer committed a mistake!
    createBins = fmap reverse . foldl' (\(sm, bs) w -> (sm + w, (sm + w) : bs)) (0, [])  -- this is atrocious and needs to be refactored
                                                                                         -- what does it do???  the 'fmap reverse' is very confusing -- maybe change to:  'reverse . snd'
                                                                                         -- sm is the current sum, bs is the bins that have bin created, w is the current relative width

------------------------------------------------------------------------------------------------
-- general framework for selection, bringing together grouped quadunit selection, selection method (best vs. probability)

-- this type signature is ugly .... can it be refactored?
genericSelect :: Grouper a  ->  (GridPoint -> b)  ->  (((t, t1) -> t1) -> [(a, b)] -> [(a, b1)])  ->  Schedule  ->  Schedule
genericSelect grpr f selector sched = 
  let
    points = getPoints sched                                                    -- 0) strip off Schedule context
    groupedPts = (getGrouper grpr) points                                       -- 1) group points
    weightedPts = map (\pt -> (pt, f $ (getGridPoint grpr) pt)) groupedPts      -- 2) apply weighting function to each
    selectedPts = map fst $ selector snd weightedPts                            -- 3) select from the points
    ungroupedPts = (getUngrouper grpr) selectedPts                              -- 4) ungroup the selected points
  in
    newSchedule ungroupedPts                                                    -- 5) rewrap points in the Schedule context




