module Flock where

import Data.List
import Octree
import Debug.Trace
import Control.Parallel

type Vector = (Float, Float, Float)
data Fishy = Fishy { 
                     position :: Vector,
                     velocity :: Vector 
                   } deriving Show

data FlockingParams = FlockingParams {
                                       timeStep :: Int,
                                       neighborSearchRadius :: Float
                                     } deriving Show

data Flock = Flock { 
                     fishies :: [Fishy], 
                     time :: Int,
                     params :: FlockingParams 
                   } deriving Show

scaleVector :: Float -> Vector -> Vector
scaleVector factor (x, y, z) = (x * factor, y * factor, z * factor)

subVector = locationDiff
addVector = locationSum

scalarMul :: Float -> Vector -> Vector
scalarMul m (x, y, z) = (x * m, y * m, z * m)

unit :: Vector -> Vector
unit v = scalarMul (1.0 / norm v) v

norm :: Vector -> Float
norm (x, y, z) = sqrt (x**2.0 + y**2.0 + z**2.0)

accel = 0.0025
maxV  = 0.2

runFishy :: FlockingParams -> Fishy -> [Fishy] -> Fishy
runFishy ps (Fishy p v) ns = Fishy p' v'
  where 
    p' = addVector p $ scaleVector ((fromIntegral $ timeStep ps) / 1000.0) v'
    v' = if null ns || norm toCentroid < 0.1
           then v
           else let tryV = addVector v $ scalarMul accel toCentroidUnit
                    tryVNorm = norm tryV
                 in if tryVNorm > maxV
                      then scalarMul (maxV / tryVNorm) tryV
                      else tryV
    toCentroidUnit = unit toCentroid
    toCentroid = subVector neighborsCentroid p
    neighborsCentroid = (sx / n, sy / n, sz / n)
      where n = fromIntegral (length ns) :: Float
            (sx, sy, sz) = 
              foldl' (\(sx', sy', sz') (Fishy (x, y, z) _) -> (sx' + x, sy' + y, sz' + z))
                     (0.0, 0.0, 0.0) ns

runFlock :: Flock -> Flock
runFlock (Flock fs t ps) = Flock newFish newTime ps
  where
    oct = fromList $ map (\f@(Fishy p _) -> OctreeDatum f p) fs
    runFishy' = runFishy ps
    newFish = map f fs
      where f x@(Fishy p v) = runFishy' x $ sphereLookup p (neighborSearchRadius ps) oct
    (FlockingParams timeStep' _) = ps
    newTime = t + timeStep'
