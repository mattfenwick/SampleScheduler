{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Formatter (
	varian,
	bruker,
	custom,
	json,
	toolkit,
	separateTransients
) where


import Model
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.RJson as R
import qualified GHC.Exts as E
--import qualified Data.Ord as O


-- ignore the number of transients; one coordinates/quadunit per line
-- 0-indexed
varian :: Schedule -> String
varian (Schedule _ pts) = concat $ L.intersperse "\n" formattedPts
  where
    formattedPts = map (sprint . decrementCoordinates . fst) $ M.toList pts
    decrementCoordinates (Point gp qus) = Point (map (flip (-) 1) gp) qus


-- ignore quadrature, transients; print out unique coordinates; one number per line
-- 1-indexed
bruker :: Schedule -> String
bruker (Schedule _ pts) = concat $ L.intersperse "\n" $ map show coordinates
  where
    coordinates = concat $ removeDuplicates gridPoints                       -- put all integers in a single list (each integer corresponds to a line)
    removeDuplicates = S.toList  .  S.fromList                          
    gridPoints = map (gridPoint . fst) $ M.toList pts                        -- unwrap grid points from Schedule, Map, tuple, Point contexts


-- ignore transients; all quadunits of a coordinates in one line
-- 1-indexed
toolkit :: Schedule -> String
toolkit (Schedule _ pts) = concat $ L.intersperse "\n" $ map lineForm ptlines
  where
    lineForm (gp, qus) = concat $ L.intersperse " " (sprint gp : (map sprint qus))    -- put a space between each coordinate, QuadUnit, all together on one line
    ptlines = map (\pts -> (gridPoint $ head pts, map quadUnit pts)) grouped          -- turn a group of points into a pair of GridPoint, QuadUnits
    grouped = E.groupWith gridPoint points                                            -- group points by equality of coordinates
    points = map fst $ M.toList pts                                                   -- unwrap points from Schedule, Map, tuple contexts


-- one coordinates/quadunit per line
-- 1-indexed
custom :: Schedule -> String
custom = sprint


-- 1-indexed
json :: Schedule -> String
json = show . toJson


-- one transient per line; like varian format except that points may be repeated (to indicate multiple transients)
-- 1-indexed
separateTransients :: Schedule -> String
separateTransients (Schedule _ pts) = concat $ L.intersperse "\n" tLines -- transLine
  where
    tLines = do
      (p, t) <- M.toList pts                                                            -- unwrap points from Schedule, Map context
      rptPt <- L.genericReplicate t p                                                   -- repeat a point t times (t is the number of transients)
      return $ formatPoint rptPt                                                     
    formatPoint (Point gp qus) = concat $ L.intersperse " " [sprint gp, sprint qus]     -- put a space between each coordinate, quadunit


-------------------------------------------------

toJson :: Schedule -> R.JsonData
toJson (Schedule d pts) = R.JDObject $ M.fromList [("numDimensions", R.JDNumber $ fromInteger d),
						   ("quadraturePoints", pointsToJson pts)]
  where
    pointsToJson = R.JDArray . map ptToJson . M.toList

ptToJson :: (Point, Integer) -> R.JsonData
ptToJson ((Point coords quadUnit), t) = R.JDObject $ M.fromList [("transients", R.JDNumber $ fromInteger t),
								 ("gridPoint", gridPointToJson coords),
								 ("quadratureUnit", quadUnitToJson quadUnit)]
  where
    gridPointToJson = R.JDArray . map (R.JDNumber . fromInteger)
    quadUnitToJson = R.JDArray . map (R.JDString . show)

--------------------------------------------------

class SchedulePrint a where
  sprint :: a -> String

instance SchedulePrint Quadrature where
  sprint = show

instance SchedulePrint GridPoint where
  sprint = concat  .  L.intersperse " "  .  fmap show

instance SchedulePrint QuadUnit where
  sprint = concat . fmap sprint

instance SchedulePrint Point where
  sprint (Point coords quadUnit) = concat [sprint coords, " ", sprint quadUnit]

instance SchedulePrint Schedule where
  sprint (Schedule _ pts) = foldr comb "" $ M.toList pts
    where
      comb (pt, t) base = concat [sprint pt, " ", show t, "\n", base]


