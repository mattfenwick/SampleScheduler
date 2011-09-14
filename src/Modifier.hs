module Modifier (
	blurred
) where


import Model
import qualified Data.Map as M
import qualified Random as R

blurred :: Integer -> Int -> Schedule -> Schedule
blurred width seed (Schedule d pts) = foldl (\s (pt, t) -> addPointTrans s pt t) (Schedule d $ M.fromList []) bumpedPts
  where
    bumpedPts = map bump $ zip randomNums $ M.toList pts 	-- there may now be multiple points with the same coordinates
    bump (bp, (Point (c:cs) qu, t)) = (Point ((c + bp):cs) qu, t)
		-- this should be refactored to modify each of the coordinates with a separate bump function
		-- this should also check to make sure that 
    randomNums = map (((-) width) . abs . (flip mod (2 * width + 1))) $ R.randoms (R.mkStdGen seed)
