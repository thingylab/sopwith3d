import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Display

main = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer, WithAlphaComponent]
	createWindow "Sopwith Camel 2009"
	
	-- State
	--angle <- newIORef (0.0::GLfloat)
	--delta <- newIORef (0.1::GLfloat)
	--position <- newIORef (0.0::GLfloat, 0.0)
	myInit
	-- Callbacks
	reshapeCallback $= Just reshape
	--keyboardMouseCallback $= Just (keyboardMouse delta position)
	--idleCallback $= Just (idle angle delta)
	displayCallback $= displayWorld
	
	-- Go !
	mainLoop


-- TODO: Make sense of it and move somewhere else
myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth

   position (Light 0) $= Vertex4 20 (20) (10.0::GLfloat) 0

   ambient (Light 1) $= Color4 1 1 1 1

   lighting $= Enabled
   light (Light 0) $= Enabled
   light (Light 1) $= Enabled
   depthFunc $= Just Less

-- TODO: move
reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-1.5) 1.5 (-1.5 * hf/wf) (1.5 * hf/wf) (-10) 10
      else ortho (-1.5 * wf/hf) (1.5 * wf/hf) (-1.5) 1.5 (-10) 10
   matrixMode $= Modelview 0
   loadIdentity