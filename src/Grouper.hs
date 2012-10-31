module Grouper (
	
    Grouper
  , getGrouper
  , getGridPoint
  , getUngrouper

  , sepTransSepQuad
  , combTransSepQuad
  , combTransCombQuad

) where

import Model
import GHC.Exts       (groupWith)
import Data.List      (genericLength, genericReplicate)
import Control.Arrow  ((&&&), first)


data Grouper a = Grouper { 
      getGrouper :: ([Point] -> [a]),
      getGridPoint :: a -> GridPoint,
      getUngrouper :: ([a] -> [Point])
  }
  

sepTransSepQuad :: Grouper Point
sepTransSepQuad = Grouper {
      getGrouper   = id,
      getGridPoint = gridPoint,
      getUngrouper = id
  }


combTransCombQuad :: Grouper (GridPoint, [(QuadUnit, Integer)])
combTransCombQuad = Grouper g ggp ug
  where
    g pts = map morpher $ groupWith (gridPoint . fst) ptsByPoint
      where
        morpher mpts@((pt,t):_) = (gridPoint pt, map (first quadUnit) mpts)           -- can this be cleaned up?
        ptsByPoint = map (head &&& genericLength) $ groupWith id pts
    ggp = fst
    ug gpts = do
	(gp, qu_ts) <- gpts
	(qu, t) <- qu_ts
        [1 .. t]
	return $ makePoint gp qu

combTransSepQuad :: Grouper (Point, Integer)
combTransSepQuad = Grouper g ggp ug
  where
    g = map (head &&& genericLength) . groupWith id
    ggp = gridPoint . fst
    ug = concatMap (\(pt, t) -> genericReplicate t pt)