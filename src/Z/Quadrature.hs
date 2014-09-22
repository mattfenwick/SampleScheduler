module Z.Quadrature (

    allQuadUnits

)  where

import Z.Model
import Control.Monad             (sequence, liftM2, liftM, replicateM, join)
import Control.Monad.Instances   ()  -- for Functor instance of ((,) a)
import System.Random             (randoms, mkStdGen)



allQuadUnits :: [[Integer]] -> [Point]
allQuadUnits = concatMap allQuads


justReals :: [[Integer]] -> [Point]
justReals = map (map tuple)
  where
    tuple = flip (,) R


allQuads :: [Integer] -> [Point]
allQuads = sequence . map (\x -> map ((,) x) [R, I])


singleRandom :: Int -> [[Integer]] -> [Point]
singleRandom sd pts = 

{-
-- the state needs to be maintained over the various invocations for each point
singleRandom :: Int -> [GridPoint] -> [(GridPoint, QuadUnit)]
singleRandom sd pts = fmap (fmap (\r -> quads !! r)) gps_rands		-- pretty ugly (fmap fmap)
  where
    gps_rands = zip pts randnums		-- pair a gridpoint with a random number
    randnums = map (flip mod (length quads) . abs) $ randoms (mkStdGen sd)		-- generate random numbers which can be used as indexes into 'quads'
    quads = allQuads (head pts)		-- bad: pulls off first point to get the length, to determine how many quadrature components need to be generated (??REFACTOR??)
-}