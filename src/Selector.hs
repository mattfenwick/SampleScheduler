module Selector (
	randomPoints,
	bestByGridPoint,
	randomPointsAllQuadUnits,
	bestByGridPointAllQuadUnits,
	probByGridPoint
) where

import Model
import qualified Data.Map as M
import qualified Random as R
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Function as F


------------------------------------------------------------------------------------------------

-- randomly select n points
--	are the Int's in the type signature bad?                current answer: leave them for now, refactor to Integral/Integer if necessary later
randomPoints :: Int -> Int -> Schedule -> Schedule
randomPoints n s = genericSelect combTransSepQuad (\_ -> ()) (\_ -> selectRandomly n s)


-- select n points, using a weighting function
--	what if the schedule has fewer than n points?
--	the weighting function returns an Integer; would a floating-point number be better?
bestByGridPoint :: Int -> (GridPoint -> Integer) -> Schedule -> Schedule
bestByGridPoint n f = genericSelect combTransSepQuad f (selectBest n)


-- should I have to specify 'Int'?  No!!!!
probByGridPoint :: (Fractional t, R.Random t, Ord t) => Int -> Int -> (GridPoint -> t) -> Schedule -> Schedule
probByGridPoint n s f = genericSelect combTransSepQuad f (probabilitySelection n s)


bestByGridPointAllQuadUnits :: Int -> (GridPoint -> Integer) -> Schedule -> Schedule
bestByGridPointAllQuadUnits n f = genericSelect combinedQuadUnits f (selectBest n)


randomPointsAllQuadUnits :: Int -> Int -> Schedule -> Schedule
randomPointsAllQuadUnits n s = genericSelect combinedQuadUnits (\_ -> ()) (\_ -> selectRandomly n s)

------------------------------------------------------------------------------------------------
-- selection strategies

-- apply a function to each element, and select the n elements with the best scores
-- no replacement
-- what if the input has fewer than n elements?           current answer: then all elements are selected
selectBest :: (Ord b) => Int -> (a -> b) -> [a] -> [a]
selectBest num f = take num . reverse . L.sortBy (O.comparing f)


-- no replacement
-- what if the input has fewer than n elements?           current answer: then all elements are selected
selectRandomly :: Int -> Int -> [a] -> [a]
selectRandomly num seed things = map fst $ selectBest num snd ratedPoints
  where
    ratedPoints = zip things randomNums
    randomNums = R.randoms (R.mkStdGen seed) :: [Double]


-- perform selection with replacement
probabilitySelection :: (Fractional t, R.Random t, Ord t) => Int -> Int -> (a -> t) -> [a] -> [a]
probabilitySelection num seed f orig = pickedPts
  where
    binWidths = map f orig
-- determine widths for each bin, based on evaluation of probability distribution function
    relBinWidths = map (\x -> x / (sum binWidths)) binWidths
-- scale the bin widths so that their sum is 1
    (_, bins) = fmap reverse $ foldl (\(sm, bs) w -> (sm + w, (sm + w) : bs)) (0, []) relBinWidths -- :: (Num t) => (t, [t])
-- create bins (one for each possible gridpoint in the schedule??), where each bin is an endpoint of an interval (and the startpoint is the previous bin)
    binPoints = zip bins orig
-- associate a pt (or an 'a' in the type signature) with each bin
    randNums = R.randoms $ R.mkStdGen seed -- :: [Double]
-- need random numbers between 0 and 1 to select the bins
    pickedPts = map (pickPoint binPoints) $ take num randNums
-- use the random numbers to pick the points
    pickPoint ((p, pt): rest) r
      | r <= p = pt
      | r <= 1 = pickPoint rest r
    pickPoint [] _ = error "pickedPts was misprogrammed, the last probability should always be one, but somehow something failed" 
-- if the random number is less than the sum, pick that point
-- otherwise, continue with the next (sum, point) pair

------------------------------------------------------------------------------------------------
-- general framework for selection, bringing together grouped quadunit selection, selection method (best vs. probability)

-- strategy:
--	strip off Schedule, Map contexts
--	1) group points in some way			(grouped QuadUnits, separate transients, ...)
--	2) apply weighting function to each		(none, exponential decay, product, ...)
--	3) select some of the points in some way	(best (no replacement), random (no replacement), probability (with replacement))
--	4) ungroup the selected points			(undo whatever 'group' did)
--	reassemble the points (if select all quadunits together, turn [(GridPoint, [QuadUnit])] into [Point], for example)
--	rewrap in the context of the Map and Schedule
genericSelect :: Grouper a  ->  (GridPoint -> b)  ->  (((t, t1) -> t1) -> [(a, b)] -> [(a, b1)])  ->  Schedule  ->  Schedule
genericSelect grpr fw selector (Schedule d pts) = 
  let
    points = M.toList pts							-- :: [(Point, Integer)]
    groupedPts = (grouper grpr) points						-- :: [a]		-- why doesn't this type signature work?
    weightedPts = map (\pt -> (pt, fw $ (getGridPoint grpr) pt)) groupedPts	-- :: [(a, b)]
    selectedPts = map fst $ selector snd weightedPts				-- :: [a]
    ungroupedPts = (ungrouper grpr) selectedPts 				-- :: [(Point, Integer)]
  in
    L.foldl' (\s (pt, t) -> addPointTrans s pt t) (Schedule d $ M.fromList []) ungroupedPts


----------------------------------------------------------------------------------------------------
-- groupers

data Grouper a = Grouper { grouper :: ([(Point, Integer)] -> [a]),
			getGridPoint :: a -> GridPoint,
			ungrouper :: ([a] -> [(Point, Integer)]) }

-- sepTransSepQuad
separateTransients :: Grouper Point
separateTransients = Grouper mygrouper getgp myungrouper
  where
    mygrouper = concatMap (\(pt, t) -> L.genericTake t $ repeat pt)
    getgp = gridPoint
    myungrouper = map (\pts -> (head pts, L.genericLength pts)) . L.group . L.sort

-- combTransCombQuad
combinedQuadUnits :: Grouper (GridPoint, [(QuadUnit, Integer)])
combinedQuadUnits = Grouper g gp ug
  where
    g pts = map morpher $ L.groupBy (F.on (==) (gridPoint . fst)) $ L.sortBy (O.comparing (gridPoint . fst)) pts   -- can this be cleaned up?
    morpher mpts = (gridPoint $ fst $ head mpts, map (\p -> (quadUnit $ fst p, snd p)) mpts)                       -- and this, too?
    gp = fst
    ug gpts = do
	(gp, qu_ts) <- gpts
	(qu, t) <- qu_ts
	return (Point gp qu, t)

-- combTransSepQuad
combTransSepQuad :: Grouper (Point, Integer)				-- combined transients, separate QuadUnits
combTransSepQuad = Grouper (map id) (gridPoint . fst) (map id)

-- sepTransCombQuad :: Grouper (GridPoint, [QuadUnit]) 
-- I don't think this makes any sense:
--	what if the original quadunits had different numbers of transients?



