module Selector (
	sepTransSepQuad,
	combTransSepQuad,
	combTransCombQuad,
	genericSelect,
	selectNBest,
	selectNRandomly,
	selectNProb
) where

import Model
import qualified Data.Map as M
import qualified Random as R
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Function as F
import qualified GHC.Exts as E



------------------------------------------------------------------------------------------------
-- selection strategies
-- selectNBest and selectNRandomly:  no replacement; if input has fewer than num element, all elements are selected

-- apply a function to each element, and select the n elements with the best scores
selectNBest :: (Ord b) => Int -> (a -> b) -> [a] -> [a]
selectNBest num f = take num . reverse . L.sortBy (O.comparing f)


selectNRandomly :: Int -> Int -> [a] -> [a]
selectNRandomly num seed things = map fst $ selectNBest num snd ratedPoints
  where
    ratedPoints = zip things randomNums
    randomNums = R.randoms (R.mkStdGen seed) :: [Double]


-- perform probabilistic selection with replacement
selectNProb :: (Fractional t, R.Random t, Ord t) => Int -> Int -> (a -> t) -> [a] -> [a]
selectNProb num seed f orig = pickedPts
  where
    binWidths = map f orig                                      -- determine bin widths from probability distribution function
    relBinWidths = map (\x -> x / (sum binWidths)) binWidths    -- scale bin widths so that sum is 1
    (_, bins) = createBins relBinWidths                         -- create a bin for each element
                                                                --     a bin is an endpoint of an interval (the startpoint is the previous bin)
    binPoints = zip bins orig                                   -- associate an element with each bin
    randNums = R.randoms $ R.mkStdGen seed -- :: [Double]       -- generate random numbers between 0 and 1
    pickedPts = map (pickPoint binPoints) $ take num randNums   -- use 'num' random numbers to pick bins
    pickPoint ((p, pt): rest) r
      | r <= p = pt                                             -- if the random number is less than the sum, pick that point
      | r <= 1 = pickPoint rest r                               -- otherwise, continue with the next (sum, point) pair
    pickPoint [] _ = error "expected: 0 <= x <= 1; actual: x > 1" 
    createBins = fmap reverse . L.foldl' (\(sm, bs) w -> (sm + w, (sm + w) : bs)) (0, []) -- this is atrocious and needs to be refactored
                                                                                          -- what does it do???


------------------------------------------------------------------------------------------------
-- general framework for selection, bringing together grouped quadunit selection, selection method (best vs. probability)

-- this type signature is ugly.  can it be refactored?
genericSelect :: Grouper a  ->  (GridPoint -> b)  ->  (((t, t1) -> t1) -> [(a, b)] -> [(a, b1)])  ->  Schedule  ->  Schedule
genericSelect grpr fw selector (Schedule d pts) = 
  let
    points = M.toList pts                                                       -- 0) strip off Schedule, Map contexts
    groupedPts = (grouper grpr) points                                          -- 1) group points in some way
    weightedPts = map (\pt -> (pt, fw $ (getGridPoint grpr) pt)) groupedPts     -- 2) apply weighting function to each
    selectedPts = map fst $ selector snd weightedPts                            -- 3) select some of the points in some way
    ungroupedPts = (ungrouper grpr) selectedPts                                 -- 4) ungroup the selected points
  in
    L.foldl' (\s (pt, t) -> addPointTrans s pt t) (Schedule d $ M.fromList []) ungroupedPts    -- 5) rewrap in the Schedule, Map contexts


----------------------------------------------------------------------------------------------------
-- groupers

data Grouper a = Grouper { grouper :: ([(Point, Integer)] -> [a]),
			getGridPoint :: a -> GridPoint,
			ungrouper :: ([a] -> [(Point, Integer)]) }

sepTransSepQuad :: Grouper Point
sepTransSepQuad = Grouper g ggp ug
  where
    g = concatMap (\(pt, t) -> L.genericTake t $ repeat pt)
    ggp = gridPoint
    ug = map (\pts -> (head pts, L.genericLength pts)) . L.group . L.sort


combTransCombQuad :: Grouper (GridPoint, [(QuadUnit, Integer)])
combTransCombQuad = Grouper g ggp ug
  where
    g pts = map morpher $ E.groupWith (gridPoint . fst) pts
      where
        morpher mpts@((pt,t):ps) = (gridPoint pt, map (\(pt1, t1) -> (quadUnit pt1, t1)) mpts)           -- can this be cleaned up?
    ggp = fst
    ug gpts = do
	(gp, qu_ts) <- gpts
	(qu, t) <- qu_ts
	return (Point gp qu, t)
-- t (a, b) f = (f a, b)

combTransSepQuad :: Grouper (Point, Integer)
combTransSepQuad = Grouper (map id) (gridPoint . fst) (map id)


-- I think this doesn't make any sense
--sepTransCombQuad :: Grouper (GridPoint, [QuadUnit]) 
--sepTransCombQuad = Grouper g gp ug
--  where
--    g pts = map morpher $ L.groupBy (F.on (==) (gridPoint . fst)) $ L.sortBy (O.comparing (gridPoint . fst)) pts
--    morpher mpts = (gridPoint $ fst $ head mpts, map (\(pt, t) -> L.genericTake t $ repeat $ quadUnit pt) mpts)
--    gp = fst
--    ug gpts = do
--	(gp, qus) <- gpts
--	qu <- qus
--	return (Point gp qu, 1) -- is this being unnecessarily inefficient

