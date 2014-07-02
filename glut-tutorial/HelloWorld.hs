import Graphics.UI.GLUT
import Data.IORef
import Bindings
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "WIZZARD"

  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse

  angle <- newIORef 0.0
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop
