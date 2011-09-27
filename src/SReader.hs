module SReader (
	readJson
) where


import Model
import qualified Text.RJson as R
import qualified Data.Map as M


--readJson :: (Monad m) => String -> m Schedule
readJson text = do
	mpoints <- jpoints text
	return $ newSchedule mpoints

jpoints :: (Monad m) => R.JsonData -> m [Point]
jpoints (R.JDObject mymap) = do
	(R.JDArray jpts) <- mlookup "quadraturePoints" mymap
	return $ doStuff $ map jpoint jpts

doStuff :: (Monad m) => [m Point] -> m [Point]
doStuff pts = do
  addThing 

jpoint :: (Monad m) => R.JsonData -> m Point
jpoint (R.JDObject mymap) = do
	(R.JDArray coords) <- mlookup "gridPoint" mymap
	(R.JDArray qunit) <- mlookup "quadratureUnit" mymap
	return $ makePoint (cs coords) (qu qunit)
  where
    cs = map (\(R.JDNumber n) -> floor n)
    qu = map (\(R.JDString s) -> read s)

mlookup :: (Monad m, Ord k, Show k) => k -> M.Map k a -> m a
mlookup k m = 
  let res = M.lookup k m
  in case res of 
    (Just r) -> return r
    Nothing -> fail $ concat ["couldn't find key<", show k, ">"]