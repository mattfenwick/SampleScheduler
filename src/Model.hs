{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Model (
	Schedule(Schedule), 
	sprint, 
	Quadrature(R, I), 
	Point(Point), 
	addPoint, 
	makeSchedule, 
	addManySchedules, 
	QuadUnit,
	GridPoint,
	addPointTrans, 	-- ??? not really comfortable with this being visible
	gridPoint, 	-- just for testing
	points		-- just for testing
) where

import qualified Data.Map as M
import qualified Data.List as L


--------------------------------------------------

data Quadrature =  R | I  deriving (Show, Eq, Ord, Enum, Bounded, Read)

type GridPoint = [Integer]

type QuadUnit = [Quadrature]

data Point = Point { gridPoint :: GridPoint, 
			quadUnit :: QuadUnit }  deriving  (Show, Eq, Ord)

data Schedule = Schedule {numDimensions :: Integer,
			  points :: M.Map Point Integer}  deriving (Show, Eq)

--------------------------------------------------

pointDims :: (Integral t) => Point -> t
pointDims (Point gp _) = L.genericLength gp

makePoint :: GridPoint -> QuadUnit -> Point
makePoint gp ph
	| length gp == length ph = Point gp ph
	| otherwise = error ("gridpoint and quadUnit dimensions don't match -- gp" ++ show (length gp) ++", quadUnit " ++ show (length ph))

--------------------------------------------------

addPoint :: Schedule -> Point -> Schedule
addPoint (Schedule d pts) pt
	| d /= pointDims pt = error ("bad number of dimensions in point -- wanted " ++ show d ++ ", got " ++ show (pointDims pt))
	| otherwise = Schedule d $ M.insert pt trans pts
		where
			trans = 1 + (extract $ M.lookup pt pts)
			extract Nothing = 0
			extract (Just x) = x

addPointTrans :: Schedule -> Point -> Integer -> Schedule
addPointTrans sched pt trans = foldl addPoint sched points
	where
		points = L.genericTake trans $ repeat pt

addTwoSchedules :: Schedule -> Schedule -> Schedule
addTwoSchedules osched (Schedule d pts) = foldl adder osched $ M.toList pts
	where
		adder s (pt, trans) = addPointTrans s pt trans

-- this is a partial function!  what if the list is empty???
addManySchedules :: [Schedule] -> Schedule
addManySchedules (s:scheds) = foldl addTwoSchedules s scheds

-- assumptions:
--	all GridPoints have the same dimensionality (as each other)
--	the quadrature generator produces quads of the same dimensionality (as the GridPoints)
--	there is at least one grid point (this is used to define the dimensionality)
makeSchedule :: [GridPoint] ->  ([GridPoint] -> [(GridPoint, QuadUnit)])  -> Schedule
makeSchedule gps quadgen = Schedule (L.genericLength $ head gps) $ M.fromList pointtrans
	where
		pointtrans = fmap (\x -> (x, 1)) quadpoints	-- generate transients for each quadrature point
		quadpoints = map (uncurry makePoint) quad_gps
		quad_gps = quadgen gps

--------------------------------------------------

class SchedulePrint a where
  sprint :: a -> String
  -- tojson :: a -> JSObject or something??

instance SchedulePrint Quadrature where
  sprint = show

instance SchedulePrint GridPoint where
  sprint = concat  .  L.intersperse " "  .  fmap show

instance SchedulePrint QuadUnit where
  sprint = concat . fmap sprint

instance SchedulePrint Point where
  sprint (Point coords quadUnit) = concat [sprint coords, " ", sprint quadUnit]

instance SchedulePrint Schedule where
  sprint (Schedule _ pts) = foldr comb "" $ M.toList pts
    where
      comb (pt, t) base = concat [sprint pt, " ", show t, "\n", base]

