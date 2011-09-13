module Quadrature (
	allPhases,
	justReals,
	singleRandom
)  where

import Model
import Control.Applicative
import qualified Random as R




allPhases = ctxtFreeQuad allphases

justReals = ctxtFreeQuad justreals


--------------------------------------------------

ctxtFreeQuad :: (GridPoint -> [Phase]) -> [GridPoint] -> [(GridPoint, Phase)]
ctxtFreeQuad qfunc gpoints = concat $ map someFunc gpoints -- someFunc gets a gridpt, and produces a list of tuples
  where
    someFunc gp = map ((,) gp) $ qfunc gp

--------------------------------------------------

-- the state needs to be maintained over the various invocations for each point
singleRandom :: Int -> [GridPoint] -> [(GridPoint, Phase)]
singleRandom sd pts = fmap (fmap (\r -> quads !! r)) gps_rands		-- pretty ugly (fmap fmap)
  where
    gps_rands = zip pts randnums		-- pair a gridpoint with a random number
    randnums = map (flip mod (length quads) . abs) $ R.randoms (R.mkStdGen sd)		-- generate random numbers which can be used as indexes into 'quads'
    quads = allphases (head pts)		-- bad: pulls off first point to get the length, to determine how many quadrature components need to be generated (??REFACTOR??)

--------------------------------------------------

allphases :: GridPoint -> [Phase]
allphases pt = sequence $ take (length pt) $ repeat [R, I]

justreals :: GridPoint -> [Phase]
justreals pt = [take (length pt) $ repeat R]

--------------------------------------------------
-- unused:
--twoquads2d = ctxtFreeQuad twoquads2dp

--twoquads2dp :: GridPoint -> [Phase]
--twoquads2dp _ = [[R, I], [I, R]]

