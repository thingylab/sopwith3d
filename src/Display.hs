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

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
-}

module Display(displayWorld) where

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Rendering
import OBJParser

displayWorld = do
	test <- loadOBJ "tests/untitled3.obj"
	--putStrLn $ show test
	clear [ColorBuffer, DepthBuffer]
	loadIdentity
	rotate 15 $ Vector3 (1::GLfloat) 0 0
	rotate 15 $ Vector3 0 (1::GLfloat) 0
	materialDiffuse Front $= Color4 1 1 1 1
	materialSpecular Front $= Color4 1 1 1 1
	materialShininess Front $= 5
	--renderObject Solid (Teapot 1.0)
	renderObj test
	swapBuffers