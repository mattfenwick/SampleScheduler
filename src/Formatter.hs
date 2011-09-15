{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Formatter (
	varian,
	bruker,
	custom,
	json,
	toolkit
) where


import Model
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.RJson as R
--import qualified Data.Ord as O


-- ignore the number of transients; one coordinates/quadunit per line
varian :: Schedule -> String
varian (Schedule _ pts) = foldr addNewline "" formattedPts
  where
    formattedPts = map (sprint . fst) $ M.toList pts
    addNewline b n = concat [b, "\n", n]


-- ignore quadrature, transients; print out unique coordinates; one number per line
bruker :: Schedule -> String
bruker (Schedule _ pts) = foldr (++) "" $ L.intersperse "\n" coordinates
  where
    coordinates = concat $ map (map show) uniquePoints 
    uniquePoints = S.toList $ S.fromList $ map (gridPoint . fst) $ M.toList pts


-- ignore transients; all quadunits of a coordinates in one line
-- this may be the worst code I've ever seen; please PLEASE !!PLEASE!! refactor soon 
-- the comments are bad, too
toolkit :: Schedule -> String
toolkit (Schedule _ pts) = concat $ L.intersperse "\n" formattedLines
  where
    -- extract points
    -- group by equality of coordinates
    -- one line for each group of coordinates
    formattedLines = fmap (\(gp, qus) -> concat $ L.intersperse " " (sprint gp : (fmap sprint qus)) ) ptlines
    ptlines = fmap (\(pt:pts) -> (gridPoint pt, fmap quadUnit (pt:pts))) grouped :: [(GridPoint, [QuadUnit])]
    grouped = L.groupBy compFunc $ fmap fst $ M.toList pts
    compFunc x y = gridPoint x == gridPoint y


-- one coordinates/quadunit per line
custom :: Schedule -> String
custom = sprint


json :: Schedule -> String
json = show . toJson

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


