import Model
import Control.Applicative


uniformGrid2d :: (Enum t) => (t, t) -> (t, t) -> [[t]]
uniformGrid2d (xl, xh) (yl, yh) = liftA3 (\x y z -> x : y : z) [xl .. xh] [yl .. yh] [[]]

uniformSchedule2d :: Schedule
uniformSchedule2d = foldl addPoint (Schedule 2 $ fromList []) [makePoint cs | cs <- uniformGrid2d (1, 10) (1,5)]
	where
		makePoint (x:y:_) = Point [x,y] [R, R]

-- --------------------------

twoquads2d :: a -> [Phase]
twoquads2d _ = [[R, I], [I, R]]

allquads :: [a] -> [Phase]
allquads pt = sequence $ take (length pt) $ repeat [R, I]

--singlerandom :: seed -> [a] -> [Phase]
--singlerandom pt sd =  ???

allreals :: [a] -> [Phase]
allreals pt = [take (length pt) $ repeat R]

-- --------------------------
gridPoints = uniformGrid2d (1,3) (2,5)

sched_eg = makeSchedule gridPoints twoquads2d

printit :: Schedule -> IO ()
printit s = do
	putStr $ sprint s
	return ()