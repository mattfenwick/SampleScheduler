module Quadrature (

    allQuadUnits
  , justReals
  , singleRandom

)  where

import Model
import Control.Monad             (sequence, liftM2, replicateM)
import Control.Monad.Instances   ()  -- for Functor instance of ((,) a)
import System.Random             (randoms, mkStdGen)



allQuadUnits :: [GridPoint] -> [(GridPoint, QuadUnit)]
allQuadUnits pts = liftM2 (,) pts (allQuads $ head pts)


justReals :: [GridPoint] -> [(GridPoint, QuadUnit)]
justReals pts = liftM2 (,) pts [replicate (length $ head pts) R]


allQuads :: GridPoint -> [QuadUnit]
allQuads pt = replicateM (length pt) [R, I]

--------------------------------------------------

-- the state needs to be maintained over the various invocations for each point
singleRandom :: Int -> [GridPoint] -> [(GridPoint, QuadUnit)]
singleRandom sd pts = fmap (fmap (\r -> quads !! r)) gps_rands		-- pretty ugly (fmap fmap)
  where
    gps_rands = zip pts randnums		-- pair a gridpoint with a random number
    randnums = map (flip mod (length quads) . abs) $ randoms (mkStdGen sd)		-- generate random numbers which can be used as indexes into 'quads'
    quads = allQuads (head pts)		-- bad: pulls off first point to get the length, to determine how many quadrature components need to be generated (??REFACTOR??)


--------------------------------------------------
-- unused:
--twoquads2d = ctxtFreeQuad twoquads2dp

--twoquads2dp :: GridPoint -> [QuadUnit]
--twoquads2dp _ = [[R, I], [I, R]]

