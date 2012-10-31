{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Formatter (

    varian
  , bruker
  , toolkit

  , custom
  , json
  , separateTransients

) where


import Model
import Grouper
import Data.Map       (fromList)
import Data.List      (intersperse, sort, nub)
import Text.RJson     (JsonData(..))


-- ignore the number of transients; one coordinates/quadunit per line
-- 0-indexed
varian :: Schedule -> String
varian sched = concat $ intersperse "\n" formattedPts
  where
    formattedPts = map (sprint . decrementCoordinates) $ nub $ getPoints sched
    decrementCoordinates pt = makePoint (map (flip (-) 1) $ gridPoint pt) $ quadUnit pt


-- ignore quadrature, transients; print out unique coordinates; one number per line
-- 1-indexed
bruker :: Schedule -> String
bruker sched = concat $ intersperse "\n" $ map show coordinates
  where
    coordinates = concat $ nub gridPoints                 -- put all integers in a single list (each integer corresponds to a line)
    gridPoints = map gridPoint $ getPoints sched          -- unwrap grid points from Schedule, Map, tuple, Point contexts


-- ignore transients; all quadunits of a coordinates in one line
-- 1-indexed
toolkit :: Schedule -> String
toolkit sched = concat $ intersperse "\n" $ map lineForm ptlines
  where
    lineForm (gp, qus) = concat $ intersperse " " (sprint gp : (map sprint qus))    -- put a space between each coordinate, QuadUnit, all together on one line
    ptlines = map (fmap (map fst)) pointTransients                                  -- get rid of the transients, keeping just the GridPoint and the QuadratureUnits
    pointTransients = (getGrouper combTransCombQuad) $ getPoints sched              -- group the points into [(GridPoint, [(QuadratureUnit, Transients)])]


-- one coordinates/quadunit per line
-- 1-indexed
custom :: Schedule -> String
custom = sprint


-- one transient per line; like varian format except that points may be repeated (to indicate multiple transients)
-- 1-indexed
separateTransients :: Schedule -> String
separateTransients sched = concat $ intersperse "\n" tLines
  where
    tLines = map formatPoint $ sort $ getPoints sched            -- unwrap points from Schedule, Map context
    formatPoint pt = concat $ intersperse " " [sprint $ gridPoint pt, sprint $ quadUnit pt]     -- put a space between each coordinate, quadunit


-- 1-indexed
json :: Schedule -> String
json = show . toJson

-------------------------------------------------

toJson :: Schedule -> JsonData
toJson sched = JDObject $ fromList [("points", pointsToJson $ getPoints sched)]
  where
    pointsToJson = JDArray . map ptToJson

ptToJson :: Point -> JsonData
ptToJson pt = JDObject $ fromList [("gridPoint", gridPointToJson $ gridPoint pt),
                                   ("quadratureUnit", quadUnitToJson $ quadUnit pt)]
  where
    gridPointToJson = JDArray . map (JDNumber . fromInteger)
    quadUnitToJson = JDArray . map (JDString . show)

--------------------------------------------------

class SchedulePrint a where
  sprint :: a -> String

instance SchedulePrint Quadrature where
  sprint = show

instance SchedulePrint GridPoint where
  sprint = concat  .  intersperse " "  .  fmap show

instance SchedulePrint QuadUnit where
  sprint = concat . fmap sprint

instance SchedulePrint Point where
  sprint pt = concat [sprint $ gridPoint pt, " ", sprint $ quadUnit pt]

instance SchedulePrint Schedule where
  sprint sched = foldr comb "" groupedPts
    where
      groupedPts = (getGrouper combTransSepQuad) $ getPoints sched
      comb (pt, t) base = concat [sprint pt, " ", show t, "\n", base]


