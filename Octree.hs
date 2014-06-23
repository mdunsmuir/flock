module Octree where

import Data.List

type OctreeLocation = (Float, Float, Float)
data OctreeDimensions = OctreeDimensions { center :: OctreeLocation, 
                                           sideLength :: Float } deriving Show
data OctreeDatum a = OctreeDatum { value :: a, location :: OctreeLocation } deriving Show

data Octree a = Octree { contents :: OctreeDatum a, dimensions :: OctreeDimensions,
                         u1, u2, u3, u4, l1, l2, l3, l4 :: Octree a }
              | Empty
                deriving Show

data Quadrant = U1 | U2 | U3 | U4 | L1 | L2 | L3 | L4 deriving Show

dimsFromLocs :: [OctreeLocation] -> OctreeDimensions
dimsFromLocs locs = OctreeDimensions (xMid, yMid, zMid) (maximum [xSide, ySide, zSide])
  where (xs, ys, zs) = unzip3 locs
        midAndSide vs = let minMax vs = (minimum vs, maximum vs)
                            (min, max) = minMax vs
                         in ((min + max) / 2.0, max - min)
        (xMid, xSide) = midAndSide xs
        (yMid, ySide) = midAndSide ys
        (zMid, zSide) = midAndSide zs

getQuadrant :: OctreeLocation -> OctreeLocation -> Quadrant
getQuadrant (xc, yc, zc) (x, y, z) =
   if z < zc -- lower, upper
     then if y < yc -- 3, 4 / 1, 2
            then if x < xc -- 3, 4
                   then L3
                   else L4
            else if x < xc -- 1, 2
                   then L1
                   else L2
     else if y < yc -- 3, 4 / 1, 2
            then if x < xc -- 3, 4
                   then U3
                   else U4
            else if x < xc -- 1, 2
                   then U1
                   else U2

accessorForQuadrant :: Quadrant -> (Octree a -> Octree a)
accessorForQuadrant U1 = u1
accessorForQuadrant U2 = u2
accessorForQuadrant U3 = u3
accessorForQuadrant U4 = u4
accessorForQuadrant L1 = l1
accessorForQuadrant L2 = l2
accessorForQuadrant L3 = l3
accessorForQuadrant L4 = l4

octantFoldStart = ([], [], [], [], [], [], [], [])
octantFold center d@(OctreeDatum _ loc) (u1s, u2s, u3s, u4s, l1s, l2s, l3s, l4s) =
  case getQuadrant center loc of
    U1 -> (d:u1s, u2s, u3s, u4s, l1s, l2s, l3s, l4s)
    U2 -> (u1s, d:u2s, u3s, u4s, l1s, l2s, l3s, l4s)
    U3 -> (u1s, u2s, d:u3s, u4s, l1s, l2s, l3s, l4s)
    U4 -> (u1s, u2s, u3s, d:u4s, l1s, l2s, l3s, l4s)
    L1 -> (u1s, u2s, u3s, u4s, d:l1s, l2s, l3s, l4s)
    L2 -> (u1s, u2s, u3s, u4s, l1s, d:l2s, l3s, l4s)
    L3 -> (u1s, u2s, u3s, u4s, l1s, l2s, d:l3s, l4s)
    L4 -> (u1s, u2s, u3s, u4s, l1s, l2s, l3s, d:l4s)

newDims (OctreeDimensions (x, y, z) sideLength) (xInc, yInc, zInc) =
  let absInc = sideLength / 4.0
   in OctreeDimensions (x + xInc * absInc, y + yInc * absInc, z + zInc * absInc) (sideLength / 2.0)

{-
y 1 2
y 3 4
  x x
-}

fromFoldResult v dims (u1s, u2s, u3s, u4s, l1s, l2s, l3s, l4s) =
  let [u1d, u2d, u3d, u4d, l1d, l2d, l3d, l4d] = map (newDims dims) [(-1.0, 1.0, 1.0),
                                                                     (1.0, 1.0, 1.0),
                                                                     (-1.0, -1.0, 1.0),
                                                                     (1.0, -1.0, 1.0),
                                                                     (-1.0, 1.0, -1.0),
                                                                     (1.0, 1.0, -1.0),
                                                                     (-1.0, -1.0, -1.0),
                                                                     (1.0, -1.0, -1.0)]
  in  Octree v dims 
      (fromList' u1d u1s)
      (fromList' u2d u2s)
      (fromList' u3d u3s)
      (fromList' u4d u4s)
      (fromList' l1d l1s)
      (fromList' l2d l2s)
      (fromList' l3d l3s)
      (fromList' l4d l4s)


fromList' :: OctreeDimensions -> [OctreeDatum a] -> Octree a
fromList' _ [] = Empty
fromList' dims (x:[]) = Octree x dims
                        Empty Empty Empty Empty Empty Empty Empty Empty
fromList' dims@(OctreeDimensions center _) (x:xs) = 
  fromFoldResult x dims $ foldr (octantFold center) octantFoldStart xs

fromList :: [OctreeDatum a] -> Octree a
fromList lst = fromList' dims lst
  where dims = dimsFromLocs $ map location lst

pointLookup :: OctreeLocation -> Octree a -> [a]
pointLookup _ Empty = []
pointLookup loc oct = if location (contents oct) == loc
                     then value (contents oct) : pointLookup loc (nextOct oct)
                     else pointLookup loc $ nextOct oct
  where nextOct = accessorForQuadrant $ getQuadrant (center $ dimensions oct) loc
