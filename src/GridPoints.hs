module GridPoints () where

import Model
import Control.Applicative


uniformGrid2d :: (Integer, Integer) -> (Integer, Integer) -> [GridPoint]
uniformGrid2d (xl, xh) (yl, yh) = sequence [[xl .. xh], [yl .. yh]]

-- randomGrid2d :: (Integer, Integer) -> (Integer, Integer) -> Integer -> [GridPoint]
-- randomGrid2d (xl, xh) (yl, yh) totalpoints = random number generator ...

