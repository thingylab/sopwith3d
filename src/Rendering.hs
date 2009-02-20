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

module Rendering(
					renderObj,
					ObjLine(ObjV, ObjVn, ObjF, OtherStuff)) where

import Graphics.UI.GLUT
import Control.Monad

data ObjFace = FaceTriangle [Vertex3 GLfloat]
	| FaceQuad [Vertex3 GLfloat]
	deriving Show

data ObjLine = ObjV (Vertex3 GLfloat)
	| ObjVn (Normal3 GLfloat)
	| ObjF [Int] [Int]					-- list of vertices, list of normals
	| OtherStuff						-- ignored lines
	deriving Show

{-
	All these lookups in renderFace are very inefficient.
	TODO: transform the ObjLine object first in a friendlier list
	that can easily be mapM_-ed. Also, use only triangulated surfaces.
	
	Note: faces should be set to smooth in blender to get the proper normals.
-}
renderObj (vs,ns, fs) =
	mapM_ (renderFace vs ns) fs

renderFace vs ns f@(ObjF vi ni) =
	case length vi of
		3 -> renderPrimitive Triangles $ do
			--mapM_ vertex vertices
			--mapM_ normal normals
			normal $ normals !! 0
			vertex $ vertices !! 0
			normal $ normals !! 1
			vertex $ vertices !! 1
			normal $ normals !! 2
			vertex $ vertices !! 2
		4 -> renderPrimitive Quads $ do
			mapM_ vertex vertices
			mapM_ normal normals
		where
			vertices = map (findVertices vs) vi
			normals = map (findNormals ns) ni

findVertices vs vn =
	v
	where
		ObjV v = vs !! (vn - 1)

findNormals ns nn =
	n
	where
		ObjVn n = ns !! (nn - 1)