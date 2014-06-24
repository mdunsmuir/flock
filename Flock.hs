module Flock where

import Octree

type Vector = (Float, Float, Float)
data Fishy = Fishy { 
                     position :: Vector,
                     velocity :: Vector 
                   } deriving Show

data FlockingParams = FlockingParams {
                                       timeStep :: Float,
                                       neighborSearchRadius :: Float
                                     } deriving Show

data Flock = Flock { 
                     fishies :: [Fishy], 
                     params :: FlockingParams 
                   } deriving Show

scaleVector :: Float -> Vector -> Vector
scaleVector factor (x, y, z) = (x * factor, y * factor, z * factor)

subVector = locationDiff
addVector = locationSum

runFishy :: FlockingParams -> Fishy -> [Fishy] -> Fishy
runFishy ps (Fishy p v) ns = Fishy p' v
  where p' = addVector p $ scaleVector (timeStep ps) v

runFlock :: Flock -> Flock
runFlock (Flock fs ps) = Flock newFish ps
  where
    oct = fromList $ map (\f@(Fishy p _) -> OctreeDatum f p) fs
    runFishy' = runFishy ps
    newFish = map f fs
      where f x@(Fishy p v) = runFishy' x $ sphereLookup p (neighborSearchRadius ps) oct
