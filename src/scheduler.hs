import Model
import Quadrature
import GridPoints



--------------------------------------------------
-- examples
--------------------------------------------------

uniformSchedule2d :: Schedule
uniformSchedule2d = makeSchedule (uniformGrid [(3, 8), (1,5)]) allPhases

uniformS2d :: Schedule
uniformS2d = makeSchedule (uniformGrid [(3, 8), (1,5)]) allPhases

uniformS3d :: Schedule
uniformS3d = makeSchedule (uniformGrid [(1,2), (3,4), (5,6)]) justReals

crazySched bounds = addManySchedules $ map (flip makeSchedule justReals) [low, fp, lp]
  where
    low = allLowerBounds bounds
    fp = firstPoint bounds
    lp = lastPoint bounds

--------------------------------------------------

-- selectors here?

--------------------------------------------------


--------------------------------------------------

gridPoints = uniformGrid [(1,3), (2,5)]

sched_eg = makeSchedule gridPoints justReals

printit :: Schedule -> IO ()
printit s = do
	putStr $ sprint s
	return ()
