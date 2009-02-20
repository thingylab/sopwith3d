{-
Copyright (c) 2009 Pierre DOUCY

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
-}

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
	normalize $= Enabled

	position (Light 0) $= Vertex4 20 (20) (10.0::GLfloat) 0

	ambient (Light 1) $= Color4 1 1 1 1

	lighting $= Enabled
	light (Light 0) $= Enabled
	light (Light 1) $= Enabled
	shadeModel $= Smooth
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