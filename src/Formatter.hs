module Formatter (
	varian,
	bruker
) where


import Model
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
-- import qualified Text.JSON as J


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
-- toolkit :: Schedule -> String
-- toolkit (Schedule _ pts) = ???

