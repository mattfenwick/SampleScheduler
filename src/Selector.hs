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


randomPoints :: Int -> Int -> Schedule -> Schedule
randomPoints number seed (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = map fst $ take number $ L.sortBy (O.comparing snd) ratedPoints
    ratedPoints = zip (M.toList pts) randomNums
    randomNums = R.randoms (R.mkStdGen seed) :: [Double]

dimProb :: ([Integer] -> Integer) -> Int -> Schedule -> Schedule
dimProb f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = map fst $ take number $ reverse $ L.sortBy (O.comparing snd) ratedPoints     -- could be refactored
    ratedPoints = map (\(Point gp ph, t) -> ((Point gp ph, t), f gp)) $ M.toList pts

dimProbsInd :: (Integer -> Double) -> Int -> Schedule -> Schedule
dimProbsInd f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = map fst $ take number $ reverse $ L.sortBy (O.comparing snd) ratedPoints
--    filteredPts = take number $ reverse $ L.sortBy (O.comparing (\(Point gp ph, t) -> product . map f $ gp)) $ M.toList pts
    ratedPoints = map (\(Point gp ph, t) -> ((Point gp ph, t), product $ map f gp)) $ M.toList pts