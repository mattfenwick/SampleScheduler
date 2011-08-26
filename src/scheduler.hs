import Model
import Control.Applicative


uniformGrid2d :: (Enum t) => (t, t) -> (t, t) -> [[t]]
uniformGrid2d (xl, xh) (yl, yh) = liftA3 (\x y z -> x : y : z) [xl .. xh] [yl .. yh] [[]]

uniformSchedule2d :: Schedule
uniformSchedule2d = foldl addPoint (Schedule 2 $ fromList []) [makePoint cs | cs <- uniformGrid2d (1, 10) (1,5)]
	where
		makePoint (x:y:_) = Point [(x, Real), (y, Real)]


