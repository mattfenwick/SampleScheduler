module Quadrature (

    allQuadUnits
  , justReals
  , singleRandom

)  where

import Model
import Control.Monad             (sequence, liftM2, replicateM)
import System.Random             (randoms, mkStdGen)



allQuadUnits :: [GridPoint] -> [(GridPoint, QuadUnit)]
allQuadUnits []      =  []
allQuadUnits (p:ps)  =  liftM2 (,) ps (allQuads $ length p)


justReals :: [GridPoint] -> [(GridPoint, QuadUnit)]
justReals []      =  []
justReals (p:ps)  =
    let rs = replicate (length p) R
    in  map (flip (,) rs) ps


allQuads :: Int -> [QuadUnit]
allQuads = flip replicateM [R, I]


-- rolled my own b/c built in transformer-based version too obnoxious to use
newtype State s a = State {getState :: s -> (s, a)}

instance Monad (State s) where
  return x = State (\s -> (s, x))
  State f >>= g = State (\s1 -> let (s2, y) = f s1
                                in getState (g y) s2)

-- bs should be infinite ...
getQuad :: [a] -> State [b] ([a], [b])
getQuad as = State (\bs -> let len = length as 
                               in (drop len bs, (as, take len bs)))


quads :: Int -> [Quadrature]
quads = map (\x -> case x of False -> R; _ -> I) . randoms . mkStdGen


singleRandom :: Int -> [GridPoint] -> [(GridPoint, QuadUnit)]
singleRandom seed pts = 
    snd (getState (sequence $ map getQuad pts) (quads seed))
