module Z.Model (

    Schedule
  , getSchedule		

  , Quadrature(R, I)

  , Point
  , Dim

  , addPoint

) where

import Data.List          (sort)
import Data.Function      (on)



data Quadrature
      = R
      | I
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

type Dim = (Integer, Quadrature)

type Point = [Dim]

-- a schedule is a multi-set of points
newtype Schedule = 
    Schedule {getSchedule :: [Point]}
  deriving (Show)

-- equality doesn't depend on order
instance Eq Schedule where
  (==) = on (==) (sort . getSchedule)        


--------------------------------------------------

-- point to be added must have same dimensionality as schedule
addPoint :: Schedule -> Point -> Schedule
addPoint (Schedule []) newpt = Schedule [newpt]
addPoint (Schedule opts@(pt:pts)) newpt
    = let l1 = length newpt
          l2 = length pt
      in if l1 == l2
         then Schedule (newpt : opts)
         else error ("bad number of dimensions in point -- wanted " ++ show l2 ++ ", got " ++ show l1)
