import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
import System.Random
import Data.IORef
import Flock
import Draw

nFishies = 350

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize

  _window <- createWindow "Flock"
  windowSize $= Size 1024 768
  
  flock <- getFlock nFishies
  flockRef <- newIORef flock

  displayCallback $= display flockRef
  idleCallback $= Just (idle flockRef)

  mainLoop

display :: IORef Flock -> DisplayCallback
display flockRef = do
  clear [ColorBuffer]
  loadIdentity
  perspective 70 (1024.0 / 768.0) 0.1 10.0
  translate $ Vector3 0.0 0.0 (-5.0 :: GLfloat)
  (Flock fishies initTime _) <- readIORef flockRef
  nowTime <- get elapsedTime
  let dt = nowTime - initTime
  drawFishies fishies dt
  flush

idle :: IORef Flock -> IdleCallback
idle flockRef = do
  flock@(Flock _ initTime ps) <- readIORef flockRef
  let (FlockingParams timeStep _) = ps
  nowTime <- get elapsedTime
  if (nowTime - initTime) >= timeStep
    then modifyIORef' flockRef runFlock
    else return ()
  postRedisplay Nothing

getFlock :: Int -> IO Flock
getFlock nFishies = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  gen3 <- newStdGen
  let rs gen = let (x, nextGen) = randomR (-2.0, 2.0) gen
                in x : (rs nextGen)
      points = take nFishies $ zip3 (rs gen1) (rs gen2) (rs gen3) :: [(Float, Float, Float)]
      fishies' = map ((flip Fishy) (0.0, 0.0, 0.0)) points
  startTime <- get elapsedTime
  return $ Flock fishies' startTime (FlockingParams 25 0.3)
