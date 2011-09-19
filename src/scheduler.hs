import Model
import Quadrature
import GridPoints
import Selector
import Modifier
import Formatter


--------------------------------------------------
-- examples
--------------------------------------------------

uniformSchedule2d :: Schedule
uniformSchedule2d = makeSchedule (uniformGrid [(3, 8), (1,5)]) allQuadUnits

uniformS2d :: Schedule
uniformS2d = makeSchedule (uniformGrid [(3, 8), (1,5)]) allQuadUnits

uniformS3d :: Schedule
uniformS3d = makeSchedule (uniformGrid [(1,2), (3,4), (5,6)]) justReals

crazySched bounds = addManySchedules $ map (flip makeSchedule justReals) [low, fp, lp]
  where
    low = allLowerBounds bounds
    fp = firstPoint bounds
    lp = lastPoint bounds

realSched = makeSchedule (uniformGrid [(1,8), (1,8)]) justReals

filtered = randomPoints 45 10007 uniformS2d

filtered2 = bestByGridPoint product 10 uniformS2d

filtered3 = dimProbsInd (\x -> 1 / (fromInteger x)) 10 realSched

blur1 = blurred 2 17 realSched

--------------------------------------------------

-- selectors here?

--------------------------------------------------


--------------------------------------------------

gridPoints = uniformGrid [(1,3), (2,5)]

sched_eg = makeSchedule gridPoints justReals

printit :: Schedule -> IO ()
printit s = do
	putStr $ custom s
	return ()
