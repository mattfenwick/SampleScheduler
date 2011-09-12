import Model
import Control.Applicative
import qualified Data.Set as S
import qualified Random as R


--------------------------------------------------
uniformGrid :: (Enum t) => [(t, t)] -> [[t]]
uniformGrid bounds = sequence ranges
  where
    ranges = map (\(l, h) -> [l .. h]) bounds

allLowerBounds :: (Integral t) => [(t, t)] -> [[t]]
allLowerBounds bounds = filter onLowerEdge gridpoints
  where
    gridpoints = uniformGrid bounds
    onLowerEdge pt = any (\((l, _), c) -> l == c) (zip bounds pt)

firstPoint :: (Integral t) => [(t, t)] -> [[t]]
firstPoint bounds = [map fst bounds]

lastPoint :: (Integral t) => [(t, t)] -> [[t]]
lastPoint bounds = [map snd bounds]


--------------------------------------------------

uniformSchedule2d :: Schedule
uniformSchedule2d = makeSchedule (uniformGrid [(3, 8), (1,5)]) allreals

uniformS2d :: Schedule
uniformS2d = makeSchedule (uniformGrid [(3, 8), (1,5)]) allquads

uniformS3d :: Schedule
uniformS3d = makeSchedule (uniformGrid [(1,2), (3,4), (5,6)]) allreals

crazySched bounds = addManySchedules $ map (flip makeSchedule allreals) [low, fp, lp]
  where
    low = allLowerBounds bounds
    fp = firstPoint bounds
    lp = lastPoint bounds

--------------------------------------------------

-- selectors here?

--------------------------------------------------
-- this entire section probably needs to be refactored
-- for imminent refactoring of makeSchedule :

ctxtFreeQuad :: (GridPoint -> [Phase]) -> [GridPoint] -> [(GridPoint, Phase)]
ctxtFreeQuad qfunc gpoints = concat $ map someFunc gpoints -- someFunc gets a gridpt, and produces a list of tuples
  where
    someFunc gp = map ((,) gp) $ qfunc gp

-- ctxtDepQuad :: (GridPoint -> [Phase]) -> [GridPoint] -> [(GridPoint, Phase)]
-- ctxtDepQuad qfunc gpts

allQuadsFull = ctxtFreeQuad allquads

allRealsFull = ctxtFreeQuad allreals

--------------------------------------------------


twoquads2d :: GridPoint -> [Phase]
twoquads2d _ = [[R, I], [I, R]]

allquads :: GridPoint -> [Phase]
allquads pt = sequence $ take (length pt) $ repeat [R, I]

-- the state needs to be maintained over the various invocations for each point
-- probably want to refactor 'makeSchedule' in Model.hs to pass the list of gridpoints as a whole
singlerandom :: Int -> [GridPoint] -> [(GridPoint, Phase)]
singlerandom sd pts = fmap (fmap (\r -> quads !! r)) gps_rands		-- pretty ugly (fmap fmap)
  where
    gps_rands = zip pts randnums		-- pair a gridpoint with a random number
    randnums = map (flip mod (length quads) . abs) $ R.randoms (R.mkStdGen sd)		-- generate random numbers which can be used as indexes into 'quads'
    quads = allquads (head pts)			-- bad: pulls off first point to get the length, to determine how many quadrature components need to be generated (??REFACTOR??)

allreals :: GridPoint -> [Phase]
allreals pt = [take (length pt) $ repeat R]

--------------------------------------------------

gridPoints = uniformGrid [(1,3), (2,5)]

sched_eg = makeSchedule gridPoints twoquads2d

printit :: Schedule -> IO ()
printit s = do
	putStr $ sprint s
	return ()
