module Model (Schedule(Schedule), example2, sprint, Quadrature(R, I), Point(Point), addPoint, fromList, makeSchedule, addSchedules, Phase) where

import Data.Map
import qualified Data.List as L


data Quadrature =  R | I  deriving (Show, Eq, Ord, Enum, Bounded, Read)

type GridPoint = [Integer]

type Phase = [Quadrature]

data Point = Point { gridPoint :: GridPoint, 
			phase :: Phase }  deriving  (Show, Eq, Ord)

pointDims :: (Integral t) => Point -> t
pointDims (Point gp _) = L.genericLength gp

makePoint :: GridPoint -> Phase -> Point
makePoint gp ph
	| length gp == length ph = Point gp ph
	| otherwise = error "gridpoint and phase dimensions don't match"

data Schedule = Schedule {numDimensions :: Integer,
			  points :: Map Point Integer}  deriving (Show, Eq)

addPoint :: Schedule -> Point -> Schedule
addPoint (Schedule d pts) pt
	| d /= pointDims pt = error "bad number of dimensions in point"
	| otherwise = Schedule d $ insert pt trans pts
		where
			trans = 1 + (extract $ Data.Map.lookup pt pts)
			extract Nothing = 0
			extract (Just x) = x

addPointTrans :: Schedule -> Point -> Integer -> Schedule
addPointTrans sched pt trans = foldl addPoint sched points
	where
		points = L.genericTake trans $ repeat pt

addSchedules :: Schedule -> Schedule -> Schedule
addSchedules osched (Schedule d pts) = foldl adder osched $ toList pts
	where
		adder s (pt, trans) = addPointTrans s pt trans

-- assumptions:
--	all GridPoints have the same dimensionality (as each other)
--	the quadrature generator produces quads of the same dimensionality (as the GridPoints)
--	there is at least one grid point
makeSchedule :: [GridPoint] -> (GridPoint -> [Phase]) -> Schedule
makeSchedule gps quadgen = Schedule (L.genericLength $ head gps) $ fromList pointtrans
	where
		pointtrans = fmap (\x -> (x, 1)) quadpoints
		quadpoints = concat $ fmap makepoints gp_quads
		makepoints (gp, ps) = fmap (makePoint gp) ps
		gp_quads = zip gps quads
		quads = fmap quadgen gps

example = Schedule {numDimensions = 2, points = fromList [(Point [3,3] [R, I], 2),
	(Point [3,4] [R, R], 1)]}

example2 = addSchedules example example


-------------------------------------------------
class SchedulePrint a where
	sprint :: a -> String

instance SchedulePrint Quadrature where
	sprint = show

instance SchedulePrint Schedule where
	sprint (Schedule _ pts) = foldr comb "" $ toList pts
		where
			comb (pt, t) base = concat [sprint pt, " ", show t, "\n", base]

instance SchedulePrint Point where
	sprint (Point loc phase) = concat [coordinates, " ", quadrature]
		where
			coordinates = concat $ L.intersperse " " $ fmap show loc
			quadrature = concat $ fmap sprint phase
