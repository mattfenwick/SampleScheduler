module Selector (
	randomPoints,
	bestByGridPoint,
	randomPointsAllQuadUnits,
	bestByGridPointAllQuadUnits,
	probByGridPoint,
	dimProbsInd,
	example1,
	example2,
	example3,
	example4
) where

import Model
import qualified Data.Map as M
import qualified Random as R
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Function as F


------------------------------------------------------------------------------------------------

-- randomly select n points
--	what if the schedule has fewer than n points?           current answer: then all points are selected
--	are transients of a point selected individually?        current answer: no, this selects (Point, transient) pairs as a group
--	are the Int's in the type signature bad?                current answer: leave them for now, refactor to Integral/Integer if necessary later
--	can points be selected multiple times?			current answer: no 
randomPoints :: Int -> Int -> Schedule -> Schedule
randomPoints number seed (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = selectRandomly number seed (M.toList pts)


-- select n points, using a weighting function
--	what if the schedule has fewer than n points?
--	the weighting function only takes the GridPoint into account, not the QuadUnit (nor the transients)
--	transients are not selected individually
--	the weighting function returns an Integer; would a floating-point number be better?
--	after weighting, the n points with the highest weights are selected -- this is not probabilistic
bestByGridPoint :: (GridPoint -> Integer) -> Int -> Schedule -> Schedule
bestByGridPoint f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = selectBest number (f . gridPoint . fst) $ M.toList pts


dimProbsInd :: (Integer -> Double) -> Int -> Schedule -> Schedule
dimProbsInd f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = selectBest number (product . map f . gridPoint . fst) $ M.toList pts


-- type signature is a problem
--	order of parameters  IS VERY BAD -- INCONSISTENT with rest of module
-- 	should I have to specify 'Int'?  No!!!!
probByGridPoint :: (Fractional t, R.Random t, Ord t) => Int -> ([t] -> t) -> Int -> Schedule -> Schedule
probByGridPoint seed f number (Schedule d pts) = Schedule d $ M.fromList filteredPts
  where
    filteredPts = probabilitySelection number number (\(Point gp _, _) -> f $ map fromIntegral gp) $ M.toList pts

------------------------------------------------------------------------------------------------
-- looking at selecting all QuadUnits of a GridPoint together

bestByGridPointAllQuadUnits :: (GridPoint -> Integer) -> Int -> Schedule -> Schedule
bestByGridPointAllQuadUnits f number (Schedule d pts) = Schedule d pointMap
  where
    pointMap = M.fromList $ concat selectedPts  
-- put all the points on an equal footing again, and place them back in a map
    selectedPts = selectBest number (f . gridPoint . fst . head) groupedPts  
-- select those groups of points based on the function
    groupedPts = L.groupBy (F.on (==) (gridPoint . fst)) $ M.toList pts  		-- is this a problem because it's not sorted?  or does M.toList sort things?
-- group (Point, transients) together based solely on (gridPoint pt) :: [[(Point, Integer)]]

randomPointsAllQuadUnits :: Int -> Int -> Schedule -> Schedule
randomPointsAllQuadUnits number seed (Schedule d pts) = Schedule d pointMap
  where
    pointMap = M.fromList $ concat filteredPts
    filteredPts = map fst $ selectBest number snd ratedPoints
    ratedPoints = zip groupedPts randomNums
    groupedPts = L.groupBy (F.on (==) (gridPoint . fst)) $ M.toList pts 
    randomNums = R.randoms (R.mkStdGen seed) :: [Double]

------------------------------------------------------------------------------------------------

-- apply a function to each element, and select the n elements with the best scores
selectBest :: (Ord b) => Int -> (a -> b) -> [a] -> [a]
selectBest num f = take num . reverse . L.sortBy (O.comparing f)


selectRandomly :: Int -> Int -> [a] -> [a]
selectRandomly number seed things = map fst $ selectBest number snd ratedPoints
  where
    ratedPoints = zip things randomNums
    randomNums = R.randoms (R.mkStdGen seed) :: [Double]


-- perform selection with replacement
-- 	issue with replace:  what should be done with the transients?  		add them? ignore them?
--		current desired answer:  add them
-- what if the schedule has fewer than n points?  	current answer:  then stuff is just selected multiple times (which can happen anyway)
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


-- example1 = genericSelect separateTransients (selectRandomly 40 10)
example1 = genericSelect separateTransients product (selectBest 25)
-- example2 = genericSelect separateTransients (selectBest 39 (product . gridPoint))
example2 = genericSelect combinedQuadUnits product (selectBest 15)
-- example2 = genericSelect separateTransients (probabilitySelection 30000 10 ((product :: [Double] -> Double) . map fromIntegral . gridPoint))
example3 = genericSelect separateTransients ((product :: [Double] -> Double) . map fromIntegral) (probabilitySelection 1025 1700)

example4 = genericSelect comTransSepQuad product (\_ -> selectRandomly 15 8988) -- I believe 'product' is ignored

----------------------------------------------------------------------------------------------------
-- groupers

data Grouper a = Grouper { grouper :: ([(Point, Integer)] -> [a]),
			getGridPoint :: a -> GridPoint,
			ungrouper :: ([a] -> [(Point, Integer)]) }

separateTransients :: Grouper Point
separateTransients = Grouper mygrouper getgp myungrouper
  where
    mygrouper = concatMap (\(pt, t) -> L.genericTake t $ repeat pt)
    getgp = gridPoint
    myungrouper = map (\pts -> (head pts, L.genericLength pts)) . L.group . L.sort

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

comTransSepQuad :: Grouper (Point, Integer)
comTransSepQuad = Grouper (map id) (gridPoint . fst) (map id)





