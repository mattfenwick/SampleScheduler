import Data.Map
import qualified Data.List as L


data Quadrature =  Real | Imaginary  deriving (Show, Eq, Ord, Enum, Bounded, Read)


data Point = Point [(Integer, Quadrature)]  deriving  (Show, Eq, Ord)

pointDims :: (Integral t) => Point -> t
pointDims (Point ds) = L.genericLength ds


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


example = Schedule {numDimensions = 2, points = fromList [(Point [(3,Real),(3,Imaginary)],2),
	(Point [(3,Real),(4,Real)],1)]}

example2 = addSchedules example example


-------------------------------------------------
class SchedulePrint a where
	sprint :: a -> String

instance SchedulePrint Quadrature where
	sprint Real = "R"
	sprint _ = "I"

instance SchedulePrint Schedule where
	sprint (Schedule _ pts) = foldr comb "" $ toList pts
		where
			comb (pt, t) base = concat [sprint pt, " ", show t, "\n", base]

instance SchedulePrint Point where
	sprint (Point loc) = concat [coordinates, " ", quadrature]
		where
			coordinates = concat $ L.intersperse " " $ fmap (show . fst) loc
			quadrature = concat $ fmap (sprint . snd) loc

-----------------------------------------------------
-- need to import Control.Applicative
--uniformGrid2d :: (Enum t) => (t, t) -> (t, t) -> [[t]]
--uniformGrid2d (xl, xh) (yl, yh) = liftA3 (\x y z -> x : y : z) [xl .. xh] [yl .. yh] [[]]

--uniformSchedule2d :: Schedule
--uniformSchedule2d = ???


