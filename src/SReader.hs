module SReader (
	readJson
) where


import Model
import qualified Text.RJson as R
import qualified Data.Map as M


--readJson :: (Monad m) => String -> m Schedule
readJson text = do
	d <- dimensions text
	mpoints <- jpoints text
	return $ Schedule d (M.fromList mpoints)


dimensions :: (Monad m) => R.JsonData -> m Integer
dimensions (R.JDObject mymap) = do
	(R.JDNumber n) <- mlookup "numDdimensions" mymap
	return $ floor n
dimensions _ = fail "dimensions must be in a json object"

jpoints :: (Monad m) => R.JsonData -> m [(Point, Integer)]
jpoints (R.JDObject mymap) = do
	(R.JDArray jpts) <- mlookup "quadraturePoints" mymap
	pts <- map jpoint jpts -- I think I'm trying to use the list monad here, and I can't do that
	return pts

jpoint :: (Monad m) => R.JsonData -> m (Point, Integer)
jpoint (R.JDObject mymap) = do
	(R.JDArray coords) <- mlookup "gridPoint" mymap
	(R.JDArray qunit) <- mlookup "quadratureUnit" mymap
	(R.JDNumber t) <- mlookup "transients" mymap
	return ((uncurry Point) (csandqs coords qunit), floor t)
  where
    csandqs coords qunit = (map (\(R.JDNumber n) -> floor n) coords, map (\(R.JDString s) -> read s) qunit)

mlookup :: (Monad m, Ord k, Show k) => k -> M.Map k a -> m a
mlookup k m = 
	let res = M.lookup k m
	in case res of 
		(Just r) -> return r
		Nothing -> fail $ concat ["couldn't find key<", show k, ">"]