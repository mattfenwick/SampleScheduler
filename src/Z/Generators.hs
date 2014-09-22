module Z.Generators (

    uniform
  , aligned
    
) where



uniform :: (Enum t) => [(t, t)] -> [[t]]
uniform = sequence . map (uncurry enumFromTo)


-- given two "points", do any of the corresponding "fields" match?
-- i.e. [1, 2, 4] matches [3, 2, 1], but not [4, 1, 2]
isAligned :: Eq t => [t] -> [t] -> Bool
isAligned lows = or . zipWith (==) lows


aligned :: Eq t => [t] -> [[t]] -> [[t]]
aligned = filter . isAligned

