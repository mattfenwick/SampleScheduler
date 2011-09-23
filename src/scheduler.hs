import Model
import Quadrature
import GridPoints
import Selector
import Modifier
import Formatter
import ExSelectors
import qualified Data.Foldable as F
import qualified Data.Monoid as Md
import qualified Data.Map as M


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

filtered2 = bestByGridPoint 10 product uniformS2d

-- filtered3 = dimProbsInd (\x -> 1 / (fromInteger x)) 10 realSched -- function removed

blur1 = blurred 2 17 realSched

smany = addManySchedules $ take 3 $ repeat filtered

sodd = addManySchedules [filtered, filtered2, blur1, uniformS2d ]

halts = makeSchedule (halton [(1,64), (1,64)] 300) (singleRandom 17)

haltsP = probByGridPoint 1000 4 ((product :: [Double] -> Double) . map fromInteger) halts

haltsG = bestByGridPoint 100 product halts

--------------------------------------------------

-- selectors here?

--------------------------------------------------
-- miscellaneous functions

totalTime :: Schedule -> Integer
totalTime = Md.getSum . F.foldMap Md.Sum . points

uniquePoints :: Schedule -> Integer
uniquePoints = toInteger . M.size . points

--------------------------------------------------

gridPoints = uniformGrid [(1,3), (2,5)]

sched_eg = makeSchedule gridPoints justReals

printit :: Schedule -> IO ()
printit s = do
	putStr $ custom s
	return ()
