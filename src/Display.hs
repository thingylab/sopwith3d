module Display(displayWorld) where

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Rendering
import OBJParser

displayWorld = do
	test <- loadOBJ "tests/cubesphere.obj"
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