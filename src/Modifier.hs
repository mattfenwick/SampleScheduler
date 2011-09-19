module Modifier (
	blurred
) where


import Model
import qualified Data.Map as M
import qualified Random as R


-- 'bump' every point in every dimension a random number of units (from -width to +width)
--	currently bumps only the first dimension
--  	points can end up outside any implicit 'bounds' -- since schedules have no concept of bounds 
-- 	duplicates can occur (by gridpoints) -- thus there is a function for adding a list of points into a schedule
--		this should maybe be a function provided by the 'Model' module
--	transients are basically ignored -- ([1,1] RR, 18) is treated the same as ([1,1] RR, 1) -- all transients are 'bumped' together
blurred :: Integer -> Int -> Schedule -> Schedule
blurred width seed (Schedule d pts) = foldl (\s (pt, t) -> addPointTrans s pt t) (Schedule d $ M.fromList []) bumpedPts
  where
    bumpedPts = map bump $ zip randomNums $ M.toList pts 	-- there may now be multiple points with the same coordinates
    bump (bp, (Point (c:cs) qu, t)) = (Point ((c + bp):cs) qu, t)
		-- this should be refactored to modify each of the coordinates with a separate bump function
		-- this should also check to make sure that 
    randomNums = map (((-) width) . abs . (flip mod (2 * width + 1))) $ R.randoms (R.mkStdGen seed)
