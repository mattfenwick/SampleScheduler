{-# LANGUAGE FlexibleInstances #-}

import GridPoints
import Test.QuickCheck
import Data.List (genericLength)

instance Arbitrary ((,) Integer Integer) where -- (Integer, Integer) where
  arbitrary = (choose (1, 1000), choose (1,1000)) :: Gen (Integer, Integer)


--prop_uniformgrid :: [(Integer, Integer)] -> Bool
--prop_uniformgrid :: [(MyInt, MyInt)] -> Bool
prop_uniformgrid xs = condition ==> collect xs $ (len == size)
  where
    condition = all (\(x,y) -> y >= x && x > 0) xs
    len = genericLength (uniformGrid xs)
    size = product $ map (\(a,b) -> b - a + 1) xs

