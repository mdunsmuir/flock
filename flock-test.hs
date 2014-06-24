import System.Random
import System.TimeIt
import Flock

rs gen = let (x, nextGen) = randomR (-100.0, 100.0) gen
          in x : (rs nextGen)

main = timeIt testFlock

nFishies = 5000

testFlock :: IO ()
testFlock = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  gen3 <- newStdGen
  let points = take nFishies $ zip3 (rs gen1) (rs gen2) (rs gen3) :: [(Float, Float, Float)]
      fishies' = map ((flip Fishy) (1.0, 1.0, 1.0)) points
      flock = Flock fishies' (FlockingParams 0.25 25.0)
      newFlock = runFlock flock
  putStrLn $ show $ head $ fishies flock
  putStrLn $ show $ head $ fishies flock
  
