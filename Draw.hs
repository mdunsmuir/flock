module Draw where

import Control.Monad
import Graphics.Rendering.OpenGL
import Flock
import Debug.Trace

drawFishies :: [Fishy] -> Int -> IO ()
drawFishies fs dt = forM_ fs drawFishy
  where
    drawFishy (Fishy p@(x, y, z) v) = 
      let (x', y', z') = addVector p $ scalarMul ((fromIntegral dt) / 1000.0) v
       in  preservingMatrix $ do translate $ Vector3 (realToFrac x')
                                                     (realToFrac y')
                                                     (realToFrac z' :: GLfloat)
                                 cube 0.015

-- cube drawing stuff

vertices :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
vertices = mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z)
 
cube :: GLfloat -> IO ()
cube w = do
  renderPrimitive Quads $ vertices
    [ 
      (w, w, w),
      (w, w, -w),
      (w, -w, -w),
      (w, -w, w),
      (w, w, w),
      (w, w, -w),
      (-w, w, -w),
      (-w, w, w),
      (w, w, w),
      (w, -w, w),
      (-w, -w, w),
      (-w, w, w),
      (-w, w, w),
      (-w, w, -w),
      (-w, -w, -w),
      (-w, -w, w),
      (w, -w, w),
      (w, -w, -w),
      (-w, -w, -w),
      (-w, -w, w),
      (w, w, -w),
      (w, -w, -w),
      (-w, -w, -w),
      (-w, w, -w) 
    ]
