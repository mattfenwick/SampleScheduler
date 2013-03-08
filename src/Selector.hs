module Selector (

    genericSelect
  , sortWith
  , shuffle
  , sortWithRand
  , sortWithRandRep

) where

import Model
import Grouper
import System.Random  (randoms, mkStdGen)
import Data.List      (sortBy, foldl')
import Data.Ord       (comparing)
import GHC.Exts       (sortWith)



randomNums :: Int -> [Double]
randomNums = randoms . mkStdGen


sortZip :: Ord b => [b] -> [a] -> [a]
sortZip bs = map snd . sortBy (comparing fst) . zip bs


shuffle :: Int -> [a] -> [a]
shuffle seed = sortZip (randomNums seed)


sortWithRand :: (a -> Double) -> Int -> [a] -> [a]
sortWithRand f seed xs = sortZip ratedPoints xs
  where
    ratedPoints = map (\(r, t) -> r * f t) $ zip (randomNums seed) xs


-- perform probabilistic selection with replacement
sortWithRandRep :: (a -> Double) -> Int -> [a] -> [a]
sortWithRandRep _ _ [] = []
sortWithRandRep f seed orig = pickedPts
  where
    binWidths = map f orig                                      -- determine bin widths from probability distribution function
    relBinWidths = map (\x -> x / (sum binWidths)) binWidths    -- scale bin widths so that sum is 1
    (_, bins) = createBins relBinWidths                         -- create a bin for each element
                                                                --     a bin is an endpoint of an interval (the startpoint is the previous bin)
    binPoints = zip bins orig                                   -- associate an element with each bin
    randNums = randomNums seed                                  -- generate random numbers between 0 and 1
    pickedPts = map (pickPoint binPoints) randNums              -- use 'num' random numbers to pick bins
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
