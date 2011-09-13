module Model (
	Schedule(Schedule), 
	example2, 
	sprint, 
	Quadrature(R, I), 
	Point(Point), 
	addPoint, 
	makeSchedule, 
	addManySchedules, 
	Phase,
	GridPoint
) where

import qualified Data.Map as M
import qualified Data.List as L

--------------------------------------------------


data Quadrature =  R | I  deriving (Show, Eq, Ord, Enum, Bounded, Read)

type GridPoint = [Integer]

type Phase = [Quadrature]

data Point = Point { gridPoint :: GridPoint, 
			phase :: Phase }  deriving  (Show, Eq, Ord)

data Schedule = Schedule {numDimensions :: Integer,
			  points :: M.Map Point Integer}  deriving (Show, Eq)

--------------------------------------------------

pointDims :: (Integral t) => Point -> t
pointDims (Point gp _) = L.genericLength gp

makePoint :: GridPoint -> Phase -> Point
makePoint gp ph
	| length gp == length ph = Point gp ph
	| otherwise = error ("gridpoint and phase dimensions don't match -- gp" ++ show (length gp) ++", phase " ++ show (length ph))

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
makeSchedule :: [GridPoint] ->  ([GridPoint] -> [(GridPoint, Phase)])  -> Schedule
makeSchedule gps quadgen = Schedule (L.genericLength $ head gps) $ M.fromList pointtrans
	where
		pointtrans = fmap (\x -> (x, 1)) quadpoints	-- generate transients for each quadrature point
		quadpoints = map (uncurry makePoint) quad_gps
		quad_gps = quadgen gps

--------------------------------------------------

example = Schedule {numDimensions = 2, points = M.fromList [(Point [3,3] [R, I], 2),
	(Point [3,4] [R, R], 1)]}

example2 = addTwoSchedules example example


-------------------------------------------------
class SchedulePrint a where
	sprint :: a -> String

instance SchedulePrint Quadrature where
	sprint = show

instance SchedulePrint Schedule where
	sprint (Schedule _ pts) = foldr comb "" $ M.toList pts
		where
			comb (pt, t) base = concat [sprint pt, " ", show t, "\n", base]

instance SchedulePrint Point where
	sprint (Point loc phase) = concat [coordinates, " ", quadrature]
		where
			coordinates = concat $ L.intersperse " " $ fmap show loc
			quadrature = concat $ fmap sprint phase
