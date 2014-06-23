import System.Random
import System.TimeIt
import Octree

junk = ["foo", "bar", "baz", "bollards", "booze", "bongs", "butts"]
values = cycle [(a, b, c, d) | a <- junk, b <- junk, c <- junk, d <- junk]

rs gen = let (x, nextGen) = randomR (-20.0, 20.0) gen
          in x : (rs nextGen)

main = timeIt buildTree

buildTree :: IO ()
buildTree = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  gen3 <- newStdGen
  let points = take 2500 $ zip3 (rs gen1) (rs gen2) (rs gen3) :: [(Float, Float, Float)]
      locatedValues = map (\(a, b) -> OctreeDatum a b) $ zip values points
      oct = fromList locatedValues
  mapM_ (putStrLn . show . ((flip pointLookup) oct)) $ take 1000 points
